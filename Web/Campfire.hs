--------------------------------------------------------------------
-- |
-- Module      : Web.Campfire
-- Description : Toplevel module for the Campfire API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : not
-- Portability: portable
--
-- Toplevel module for the Campfire API. Should ideally cover all calls that the
-- Campfire REST API exposes at a high level.
-- 
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web.Campfire ( getRooms, 
                      getRoom,
                      getPresence,
                      getMe,
                      getUser,
                      speak,
                      getRecentMessages,
                      getUploads,
                      getUpload,
                      search,
                      getTodayTranscript
                    ) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack, length)
import Web.Campfire.Types
import Web.Campfire.Monad

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.Curl
import Network.Curl.Types
import Network.Curl.Code
import Network.Curl.Post
import Network.URL
import Data.Aeson
import Data.Attoparsec (parse, maybeResult, eitherResult)

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef

--------- Room Operations
getRooms :: CampfireM [Room]
getRooms = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub "rooms.json" []
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result

getRoom :: Integer -> CampfireM Room
getRoom id = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  let room = unRootRoom $ (unWrap . readResult) result
  return $ room { roomId = id }
          where path = T.concat ["room/", i2t id, ".json"]

getPresence :: CampfireM [Room]
getPresence = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub "presence.json" []
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result


--------- User Operations
getMe :: CampfireM User
getMe = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub "users/me.json" []
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result

getUser :: Integer -> CampfireM User
getUser id = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result
            where path = T.concat ["users/", i2t id, ".json"]

--------- Message Operations
speak :: Integer -> Statement -> CampfireM (CurlCode, String)
speak roomId stmt = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  doPost key sub path stmt -- I think this is antipattern
            where path = T.concat ["room/", i2t roomId, "/speak.json"]

getRecentMessages :: Integer -> Maybe Integer -> Maybe Integer -> CampfireM [Message]
getRecentMessages id limit since_id = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path params
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
      where params           = (limitP limit) ++ (limitS since_id)
            path             = T.concat ["room/", i2t id, "/recent.json"]
            limitP (Nothing) = []
            limitP (Just l)  = [("limit", show l)]
            limitS (Nothing) = []
            limitS (Just i)  = [("since_message_id", show i)]

-- Doesn't quite parse yet
getTodayTranscript :: Integer -> CampfireM [Message]
getTodayTranscript id = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  liftIO $ putStrLn $ snd resp
  return $ (unMessages . unWrap . readResult) result
      where path = T.concat ["room/", i2t id, "/transcript.json"]

--------- Upload Operations
getUploads :: Integer -> CampfireM [Upload]
getUploads id = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  return $ (unUploads . unWrap . readResult) result
            where path = T.concat ["room/", i2t id, "/uploads.json"]

--FIXME: this may not work, getting 404s
getUpload :: Integer -> Integer -> CampfireM Upload
getUpload roomId uploadId = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  let upload = unRootUpload $ (unWrap . readResult) result
  return upload
            where path = T.concat ["room/", i2t roomId, "/messages/",
                                   i2t uploadId, "/upload.json"]

--------- Upload Operations
search :: T.Text -> CampfireM [Message]
search term = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
            where path = T.concat ["search/", encTerm, ".json"]
                  encTerm = T.pack $ encString True ok_path $ T.unpack term

--------- Helpers
i2t :: Integer -> T.Text
i2t = T.pack . show

unWrap :: (FromJSON a) => Result a -> a
unWrap (Success a) = a
unWrap (Error err) = error $ "parse error: " ++ err

doGet :: T.Text -> T.Text -> T.Text -> [(String, String)] -> CampfireM (CurlCode, String)
doGet key sub path params = liftIO $ curlGetString url opts
                     where url  = T.unpack $ cfURL path sub params
                           opts = curlOpts key


-- Yeesh
doPost :: (ToJSON a) => T.Text -> T.Text -> T.Text -> a -> CampfireM (CurlCode, String)
doPost key sub path pay = liftIO $ withCurlDo $ do
  bodyRef <- newIORef []
  h       <- initialize
  mapM_ (setopt h) $ [CurlURL url,
                      CurlNoBody False,
                      CurlFollowLocation False,
                      CurlMaxRedirs 0,
                      CurlAutoReferer False,
                      CurlVerbose True, --temporary
                      CurlPostFields [postBody],
                      CurlHttpHeaders ["Content-Type: application/json"],
                      CurlWriteFunction $ bodyFunction bodyRef] ++ (curlOpts key)
  code <- perform h
  result <- fmap (LBS8.unpack . LBS8.fromChunks . reverse) $ readIORef bodyRef
  return (code, result)
                           where postBody = T.unpack $ encodePayload pay
                                 url      = T.unpack $ cfURL path sub []

bodyFunction :: IORef [BS.ByteString] -> WriteFunction
bodyFunction r = gatherOutput_ $ \s -> do
  bs <- BS.packCStringLen s
  modifyIORef r (bs:)

encodePayload :: (ToJSON a) => a -> T.Text
encodePayload pay = T.pack $ LBS8.unpack $ encode pay

handleResponse :: (CurlCode, String) -> Either CurlCode T.Text
handleResponse (CurlOK, str) = Right $ T.pack str
handleResponse (code, _)     = Left code

cfURL :: T.Text -> T.Text -> [(String,String)] -> T.Text
cfURL path sub params = T.pack $ exportURL url
  where url        = URL { url_type   = Absolute host,
                           url_path   = T.unpack path,
                           url_params = params }
        host       = Host { protocol = HTTP True, 
                            host = T.unpack $ sub `T.append` ".campfirenow.com",
                            port = Just 443 }

curlOpts :: T.Text -> [CurlOption]
curlOpts key = [CurlUserPwd $ T.unpack key, CurlPort 443, CurlVerbose True]

--TODO proper error handling
readResult :: (FromJSON r) => Either CurlCode T.Text -> Result r
readResult (Right txt) = handleParse $ eitherResult $ parse json bs
                         where handleParse (Right obj) = fromJSON obj
                               handleParse (Left err) = error $ "Failed to parse: " ++ show err
                               bs = encodeUtf8 txt
readResult (Left code) = error $ "Unexpected Code: " ++ show code

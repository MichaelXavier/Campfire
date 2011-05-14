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
                      setRoomTopic,
                      setRoomName,
                      getMe,
                      getUser,
                      speak,
                      getRecentMessages,
                      getUploads,
                      getUpload,
                      search,
                      getTodayTranscript,
                      getTranscript
                    ) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack, length)
import Web.Campfire.Types
import Web.Campfire.Monad
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.URL (encString, ok_path)
import Data.Aeson
import Data.Attoparsec (parse, maybeResult, eitherResult)
import Network.HTTP.Enumerator
import Network.HTTP.Types (methodGet,
                           methodPut,
                           methodPost,
                           headerContentType,
                           Method(..),
                           QueryItem(..),
                           Query(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time.Calendar (Day(..), toGregorian)

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

setRoomTopic :: Integer -> T.Text -> CampfireM (Int, LBS.ByteString)
setRoomTopic id topic = updateRoom id $ RoomUpdate { updateRoomName = Nothing, 
                                                   updateRoomTopic = Just topic }

setRoomName :: Integer -> T.Text -> CampfireM (Int, LBS.ByteString)
setRoomName id name = updateRoom id $ RoomUpdate { updateRoomName = Just name, 
                                                   updateRoomTopic = Nothing }

updateRoom :: Integer -> RoomUpdate -> CampfireM (Int, LBS.ByteString)
updateRoom id update = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  doPut key sub path update
                  where path   = T.concat ["room/", i2t id, ".json"]

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
speak :: Integer -> Statement -> CampfireM (Int, LBS.ByteString)
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
            limitP (Just l)  = [("limit", Just $ BS.pack $ show l)]
            limitS (Nothing) = []
            limitS (Just i)  = [("since_message_id", Just $ BS.pack $ show i)]

getTodayTranscript :: Integer -> CampfireM [Message]
getTodayTranscript id = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
      where path = T.concat ["room/", i2t id, "/transcript.json"]

getTranscript :: Integer -> Day -> CampfireM [Message]
getTranscript id day = do
  key  <- asks cfKey
  sub  <- asks cfSubDomain
  resp <- doGet key sub path []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
                  where path = T.concat ["room/", i2t id, "/transcript/", i2t y, 
                                         "/", i2t m, "/", i2t d, ".json"]
                        (y, m, d) = toGregorian day

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
i2t :: (Integral a) => a -> T.Text
i2t = T.pack . show

unWrap :: (FromJSON a) => Result a -> a
unWrap (Success a) = a
unWrap (Error err) = error $ "parse error: " ++ err

doGet :: T.Text -> T.Text -> T.Text -> Query -> CampfireM (Int, LBS.ByteString)
doGet key sub path params = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
                     where req = genRequest key sub path params methodGet $ LBS.empty

doPost :: (ToJSON a) => T.Text -> T.Text -> T.Text -> a -> CampfireM (Int, LBS.ByteString)
doPost = postWithPayload methodPost

doPut :: (ToJSON a) => T.Text -> T.Text -> T.Text -> a -> CampfireM (Int, LBS.ByteString)
doPut = postWithPayload methodPut

postWithPayload :: (ToJSON a) => Method -> T.Text -> T.Text -> T.Text -> a -> CampfireM (Int, LBS.ByteString)
postWithPayload meth key sub path pay = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
                     where req = genRequest key sub path [] meth $ encode pay

genRequest :: T.Text -> T.Text -> T.Text -> Query -> Method -> LBS.ByteString -> Request IO
genRequest key sub path params meth pay = applyBasicAuth bkey "X" $ Request { 
  method         = methodPut,
  secure         = True,
  checkCerts     = \_ -> return True, -- uhhh
  host           = h,
  port           = 443,
  path           = encodeUtf8 path,
  queryString    = params,
  requestHeaders = headers,
  requestBody    = RequestBodyLBS pay }
                    where h       = encodeUtf8 $ T.concat [sub, ".campfirenow.com"]
                          headers = [headerContentType "application/json"]
                          bkey    = encodeUtf8 key

handleResponse :: (Int, LBS.ByteString) -> Either Int T.Text
handleResponse (200, str) = Right $ T.pack $ LBS.unpack str
handleResponse (code, _)  = Left code

--TODO proper error handling
readResult :: (FromJSON r) => Either Int T.Text -> Result r
readResult (Right txt) = handleParse $ eitherResult $ parse json bs
                         where handleParse (Right obj) = fromJSON obj
                               handleParse (Left err) = error $ "Failed to parse: " ++ show err
                               bs = encodeUtf8 txt
readResult (Left code) = error $ "Unexpected Code: " ++ show code

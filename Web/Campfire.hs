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

{-# LANGUAGE OverloadedStrings #-}
module Web.Campfire ( getRooms, 
                      getRoom,
                      getPresence,
                      setRoomTopic,
                      setRoomName,
                      joinRoom,
                      leaveRoom,
                      lockRoom,
                      unlockRoom,
                      getMe,
                      getUser,
                      speak,
                      highlightMessage,
                      unhighlightMessage,
                      getRecentMessages,
                      getUploads,
                      getUpload,
                      search,
                      getTodayTranscript,
                      getTranscript
                    ) where

import Web.Campfire.Types
import Web.Campfire.Monad

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import Data.Attoparsec (parse, eitherResult)
import Data.Time.Calendar (Day(..), toGregorian)

--import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import Network.URL (encString, ok_path)
import Network.HTTP.Enumerator
import Network.HTTP.Types (methodGet,
                           methodPut,
                           methodPost,
                           methodDelete,
                           headerContentType,
                           Method,
                           --QueryItem,
                           Query)

--------- Room Operations
getRooms :: CampfireM [Room]
getRooms = withEnv $ \key sub -> do
  resp <- doGet key sub "rooms.json" []
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result

getRoom :: Integer -> CampfireM Room
getRoom rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  let room = unRootRoom $ (unWrap . readResult) result
  return $ room { roomId = rid }
          where pth = T.concat ["room/", i2t rid, ".json"]

getPresence :: CampfireM [Room]
getPresence = withEnv $ \key sub -> do
  resp <- doGet key sub "presence.json" []
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result

setRoomTopic :: Integer -> T.Text -> CampfireM (Int, LBS.ByteString)
setRoomTopic rid topic = updateRoom rid RoomUpdate { updateRoomName  = Nothing, 
                                                     updateRoomTopic = Just topic }

setRoomName :: Integer -> T.Text -> CampfireM (Int, LBS.ByteString)
setRoomName rid name = updateRoom rid RoomUpdate { updateRoomName  = Just name, 
                                                   updateRoomTopic = Nothing }

updateRoom :: Integer -> RoomUpdate -> CampfireM (Int, LBS.ByteString)
updateRoom rid update = withEnv $ \key sub ->
  doPut key sub pth $ encode update
                  where pth   = T.concat ["room/", i2t rid, ".json"]

joinRoom :: Integer -> CampfireM (Int, LBS.ByteString)
joinRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/join.json"]

leaveRoom :: Integer -> CampfireM (Int, LBS.ByteString)
leaveRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/leave.json"]

lockRoom :: Integer -> CampfireM (Int, LBS.ByteString)
lockRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/lock.json"]

unlockRoom :: Integer -> CampfireM (Int, LBS.ByteString)
unlockRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/unlock.json"]

--------- User Operations
getMe :: CampfireM User
getMe = withEnv $ \key sub -> do
  resp <- doGet key sub "users/me.json" []
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result

getUser :: Integer -> CampfireM User
getUser rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result
            where pth = T.concat ["users/", i2t rid, ".json"]

--------- Message Operations
speak :: Integer -> Statement -> CampfireM (Int, LBS.ByteString)
speak rid stmt = withEnv $ \key sub ->
  doPost key sub pth $ encode stmt
            where pth = T.concat ["room/", i2t rid, "/speak.json"]

highlightMessage :: Integer -> CampfireM (Int, LBS.ByteString)
highlightMessage mid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
            where pth = T.concat ["messages/", i2t mid, "/star.json"]

unhighlightMessage :: Integer -> CampfireM (Int, LBS.ByteString)
unhighlightMessage mid = withEnv $ \key sub ->
  doDelete key sub pth LBS.empty
            where pth = T.concat ["messages/", i2t mid, "/star.json"]

getRecentMessages :: Integer -> Maybe Integer -> Maybe Integer -> CampfireM [Message]
getRecentMessages rid limit since_id = withEnv $ \key sub -> do
  resp <- doGet key sub pth params
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
      where params           = limitP limit ++ limitS since_id
            pth             = T.concat ["room/", i2t rid, "/recent.json"]
            limitP (Nothing) = []
            limitP (Just l)  = [("limit", Just $ BS.pack $ show l)]
            limitS (Nothing) = []
            limitS (Just i)  = [("since_message_id", Just $ BS.pack $ show i)]

getTodayTranscript :: Integer -> CampfireM [Message]
getTodayTranscript rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
      where pth = T.concat ["room/", i2t rid, "/transcript.json"]

getTranscript :: Integer -> Day -> CampfireM [Message]
getTranscript rid day = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
                  where pth = T.concat ["room/", i2t rid, "/transcript/", i2t y, 
                                         "/", i2t m, "/", i2t d, ".json"]
                        (y, m, d) = toGregorian day

--------- Upload Operations
getUploads :: Integer -> CampfireM [Upload]
getUploads rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unUploads . unWrap . readResult) result
            where pth = T.concat ["room/", i2t rid, "/uploads.json"]

--FIXME: this may not work, getting 404s
getUpload :: Integer -> Integer -> CampfireM Upload
getUpload rid uid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  let upload = unRootUpload $ (unWrap . readResult) result
  return upload
            where pth = T.concat ["room/", i2t rid, "/messages/",
                                   i2t uid, "/upload.json"]

--------- Upload Operations
search :: T.Text -> CampfireM [Message]
search term = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
            where pth = T.concat ["search/", encTerm, ".json"]
                  encTerm = T.pack $ encString True ok_path $ T.unpack term

--------- Helpers
withEnv :: (T.Text -> T.Text -> CampfireM a) -> CampfireM a
withEnv fn = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  fn key sub

i2t :: (Integral a) => a -> T.Text
i2t = T.pack . show

unWrap :: (FromJSON a) => Result a -> a
unWrap (Success a) = a
unWrap (Error err) = error $ "parse error: " ++ err

doGet :: T.Text -> T.Text -> T.Text -> Query -> CampfireM (Int, LBS.ByteString)
doGet key sub pth params = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
                  where req = genRequest key sub pth params methodGet LBS.empty

doPost :: T.Text -> T.Text -> T.Text -> LBS.ByteString -> CampfireM (Int, LBS.ByteString)
doPost = postWithPayload methodPost

doPut :: T.Text -> T.Text -> T.Text -> LBS.ByteString -> CampfireM (Int, LBS.ByteString)
doPut = postWithPayload methodPut

doDelete :: T.Text -> T.Text -> T.Text -> LBS.ByteString -> CampfireM (Int, LBS.ByteString)
doDelete = postWithPayload methodDelete

postWithPayload :: Method -> T.Text -> T.Text -> T.Text -> LBS.ByteString -> CampfireM (Int, LBS.ByteString)
postWithPayload meth key sub pth pay = liftIO $ withManager $ \manager -> do
  Response { statusCode = c, responseBody = b} <- httpLbsRedirect req manager
  return (c, b)
                     where req = genRequest key sub pth [] meth pay

genRequest :: T.Text -> T.Text -> T.Text -> Query -> Method -> LBS.ByteString -> Request IO
genRequest key sub pth params meth pay = applyBasicAuth bkey "X" Request { 
  method         = meth,
  secure         = True,
  checkCerts     = \_ -> return True, -- uhhh
  host           = h,
  port           = 443,
  path           = encodeUtf8 pth,
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

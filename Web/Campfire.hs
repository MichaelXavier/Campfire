--------------------------------------------------------------------
-- |
-- Module      : Web.Campfire
-- Description : Toplevel module for the Campfire API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
-- Toplevel module for the Campfire API operating in the CamfireM monad. 
-- Covers the entire campfire API excluding the streaming and file upload APIs.
-- Might include support for these features in the future.
-- 
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Web.Campfire
-- > import Web.Campfire.Monad
-- > import Web.Campfire.Types
-- > import Control.Monad.Reader
-- > import Data.Text (unpack)
-- > 
-- > doStuff :: CampfireM ()
-- > doStuff = do
-- >   (room:_) <- getRooms
-- >   let rid = roomId room
-- >   joinRoom rid
-- >   speak rid stmt
-- >   leaveRoom rid
-- >   return ()
-- >           where stmt = TextStatement { statementBody = "ATTENTION: I have nothing important to say" }
-- > 
-- > main :: IO ()
-- > main = do
-- >   runReaderT (unCampfireM doStuff) env
-- >   me <- runReaderT (unCampfireM getMe) env
-- >   putStrLn "Hello, my name is:"
-- >   putStrLn . unpack $ userName me
-- >        where env  = CampfireEnv { cfKey = "MYKEY", cfSubDomain = "mysubdomain"}
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
                           Query)

--------- Room Operations
-- |Get a list of rooms visible to the authenticated user.
getRooms :: CampfireM [Room]
getRooms = withEnv $ \key sub -> do
  resp <- doGet key sub "rooms.json" []
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result

-- |Get a specific room by Room ID.
getRoom :: Id -- ^ Room ID
           -> CampfireM Room
getRoom rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  let room = unRootRoom $ (unWrap . readResult) result
  return $ room { roomId = rid }
          where pth = T.concat ["room/", i2t rid, ".json"]

-- |Get a list of rooms in which the authenticated user is present.
getPresence :: CampfireM [Room]
getPresence = withEnv $ \key sub -> do
  resp <- doGet key sub "presence.json" []
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result

-- |Change the topic of a particular room.
setRoomTopic :: Id                                 -- ^ Room ID
                -> T.Text                          -- ^ New topic
                -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
setRoomTopic rid topic = updateRoom rid RoomUpdate { updateRoomName  = Nothing, 
                                                     updateRoomTopic = Just topic }

-- |Change the name of a particular room.
setRoomName :: Id                                 -- ^ Room ID
               -> T.Text                          -- ^ New room name
               -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
setRoomName rid name = updateRoom rid RoomUpdate { updateRoomName  = Just name, 
                                                   updateRoomTopic = Nothing }

updateRoom :: Id -> RoomUpdate -> CampfireM (Int, LBS.ByteString)
updateRoom rid update = withEnv $ \key sub ->
  doPut key sub pth $ encode update
                  where pth   = T.concat ["room/", i2t rid, ".json"]

-- |Causes the authenticated user to join a particular room.
joinRoom :: Id                                 -- ^ Room ID
            -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
joinRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/join.json"]

-- |Causes the authenticated user to leave a particular room.
leaveRoom :: Id                                 -- ^ Room ID
             -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
leaveRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/leave.json"]

-- |Locks a particular room.
lockRoom :: Id                                 -- ^ Roomd ID
            -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
lockRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/lock.json"]

-- |Unlocks a particular room.
unlockRoom :: Id                                 -- ^ Room ID
              -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
unlockRoom rid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
                  where pth   = T.concat ["room/", i2t rid, "/unlock.json"]

--------- User Operations

-- |Get information about the currently authenticated user.
getMe :: CampfireM User
getMe = withEnv $ \key sub -> do
  resp <- doGet key sub "users/me.json" []
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result

-- |Get information about the requested user.
getUser :: Id  -- ^ User ID
           -> CampfireM User
getUser rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result
            where pth = T.concat ["users/", i2t rid, ".json"]

--------- Message Operations

-- |Say something in a room as the currently authenticated user.
speak :: Id           -- ^ The room ID in which to speak
         -> Statement -- ^ The statement to send to the room
         -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
speak rid stmt = withEnv $ \key sub ->
  doPost key sub pth $ encode stmt
            where pth = T.concat ["room/", i2t rid, "/speak.json"]

-- |Put a star next to a message. That message will then show up in that day's highlights.
highlightMessage :: Id                                 -- ^ Message ID
                    -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
highlightMessage mid = withEnv $ \key sub ->
  doPost key sub pth LBS.empty
            where pth = T.concat ["messages/", i2t mid, "/star.json"]

-- |Remove the star next to a message.
unhighlightMessage :: Id                                 -- ^ Message ID
                      -> CampfireM (Int, LBS.ByteString) -- ^ Status code and body (may change)
unhighlightMessage mid = withEnv $ \key sub ->
  doDelete key sub pth LBS.empty
            where pth = T.concat ["messages/", i2t mid, "/star.json"]

-- |Receive a list of recent messages in a particular room.
getRecentMessages :: Id               -- ^ Room ID
                     -> Maybe Integer -- ^ Optional limit. Default is 100
                     -> Maybe Integer -- ^ Optional message ID. Setting this will retreive messages since that message was received.
                     -> CampfireM [Message]
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

-- |Get a transcript of all messages in a room for the day.
getTodayTranscript :: Id -- ^ Room ID
                      -> CampfireM [Message]
getTodayTranscript rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
      where pth = T.concat ["room/", i2t rid, "/transcript.json"]

-- |Get a transcript of all messages in a room for a particular day
getTranscript :: Id      -- ^ Room ID
                 -> Day  -- ^ Day from which to retrieve the transcript
                 -> CampfireM [Message]
getTranscript rid day = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unMessages . unWrap . readResult) result
                  where pth = T.concat ["room/", i2t rid, "/transcript/", i2t y, 
                                         "/", i2t m, "/", i2t d, ".json"]
                        (y, m, d) = toGregorian day

--------- Upload Operations

-- |Get a list of up to 5 recent uploads to a given room
getUploads :: Id -- ^ Room ID
              -> CampfireM [Upload]
getUploads rid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  return $ (unUploads . unWrap . readResult) result
            where pth = T.concat ["room/", i2t rid, "/uploads.json"]

--FIXME: this may not work, getting 404s

-- |Retrieve a particular upload from a room.
getUpload :: Id    -- ^ Room ID
             -> Id -- ^ Upload ID
             -> CampfireM Upload
getUpload rid uid = withEnv $ \key sub -> do
  resp <- doGet key sub pth []
  let result = handleResponse resp
  let upload = unRootUpload $ (unWrap . readResult) result
  return upload
            where pth = T.concat ["room/", i2t rid, "/messages/",
                                   i2t uid, "/upload.json"]

--------- Search Operations

-- |Search for messages matching a given term.
search :: T.Text -- ^ Search string
          -> CampfireM [Message]
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

i2t :: (Integral a, Show a) => a -> T.Text
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
genRequest key sub pth params meth pay = applyBasicAuth bkey "X" req
  where req = def {
          method         = meth,
          secure         = True,
          host           = h,
          port           = 443,
          path           = encodeUtf8 pth,
          queryString    = params,
          requestHeaders = headers,
          requestBody    = RequestBodyLBS pay
        } 
        h       = encodeUtf8 $ T.concat [sub, ".campfirenow.com"]
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

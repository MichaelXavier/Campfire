--------------------------------------------------------------------
-- |
-- Module      : Web.Campfire.Types
-- Description : Types returned by the Campfire API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Web.Campfire.Types ( Room(..),
                            RoomWithRoot(..),
                            Rooms(..),
                            RoomUpdate(..),
                            Message(..),
                            Messages(..),
                            MessageType(..),
                            Statement(..),
                            Sound(..),
                            User(..),
                            UserWithRoot(..),
                            UserType(..),
                            Upload(..),
                            Uploads(..),
                            UploadWithRoot(..),
                            Id,
                            CampfireTime(..) ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as AT (typeMismatch, Parser)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Data.Map as M
import Data.Text
import Data.Typeable
import Locale (defaultTimeLocale)

---------- Rooms
-- |A chat room on a Campfire site
data Room 
  = Room { roomId               :: Id,
           roomName             :: T.Text,
           roomTopic            :: Maybe T.Text, -- ^ Room topic if available
           roomMembershipLimit  :: Integer,      -- ^ Maximum number of users that may be in this room
           roomFull             :: Maybe Bool,   -- ^ May not be present depending on the API call used
           roomOpenToGuests     :: Maybe Bool,   -- ^ May not be present depending on the API call used
           roomActiveTokenValue :: Maybe T.Text, -- ^ Campfire API doesn't really specify what this means
           roomUpdatedAt        :: CampfireTime,
           roomCreatedAt        :: CampfireTime,
           roomUsers            :: Maybe [User] } deriving (Eq, Ord, Read, Show, Typeable)

 -- There is probably a better way to do this
instance FromJSON Room where
  parseJSON (Object v) = Room <$> v .:| ("id", 0)
                              <*> v .:  "name"
                              <*> v .:  "topic"
                              <*> v .:  "membership_limit"
                              <*> v .:? "full"
                              <*> v .:? "open_to_guests"
                              <*> v .:? "active_token_value"
                              <*> v .:  "updated_at"
                              <*> v .:  "created_at"
                              <*> v .:? "users"
  parseJSON _ = mzero

-- |Utility type used for extracting a Room from the root JSON object the Campfire API returns
newtype RoomWithRoot = RoomWithRoot { unRootRoom :: Room } deriving (Eq, Ord, Read, Show, Typeable)
 

instance FromJSON RoomWithRoot where
  parseJSON (Object v) = RoomWithRoot <$> v .: "room"
  parseJSON _          = mzero


-- |Utility type used for extracting a Room from the list returned by the Campfire API
newtype Rooms = Rooms { unRooms :: [Room] } deriving (Eq, Ord, Read, Show, Typeable)
 

instance FromJSON Rooms where
  parseJSON (Object v) = Rooms <$> v .: "rooms"
  parseJSON _          = mzero


-- |Modification to be made to a room.
data RoomUpdate = RoomUpdate { updateRoomName  :: Maybe T.Text, 
                               updateRoomTopic :: Maybe T.Text } deriving (Eq, Ord, Read, Show, Typeable)

--TODO: maybe omit missing fields
instance ToJSON RoomUpdate where
  toJSON RoomUpdate {updateRoomName = n, updateRoomTopic = t} =
    "room" |- object ["name" .= n, "topic" .= t]
  

---------- Messages

-- |A single line of dialog in a particular chat
data Message = Message { messageId        :: Id,
                         messageBody      :: Maybe T.Text,
                         messageRoomId    :: Id,
                         messageUserId    :: Maybe Id,
                         messageCreatedAt :: CampfireTime,
                         messageType      :: MessageType } deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON Message where
  -- consider using an ADT for type
  parseJSON (Object v) = Message <$> v .: "id"
                                 <*> v .: "body"
                                 <*> v .: "room_id"
                                 <*> v .: "user_id"
                                 <*> v .: "created_at"
                                 <*> v .: "type"
  parseJSON _          = mzero

-- |Utility type used for extracting a Message from the list returned by the Campfire API
newtype Messages = Messages { unMessages :: [Message] } deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON Messages where
  parseJSON (Object v) = Messages <$> v .: "messages"
  parseJSON _          = mzero


-- |Distinct types of messages which can be found in Campfire
data MessageType = TextMessage |
                   PasteMessage |          -- ^ Monospaced text displayed in block form
                   SoundMessage |          -- ^ Audio sound effect message
                   AdvertisementMessage |
                   AllowGuestsMessage |
                   DisallowGuestsMessage |
                   IdleMessage |
                   KickMessage |           -- ^ Message indicating that a user was kicked out
                   LeaveMessage |
                   SystemMessage |
                   TimestampMessage |
                   TopicChangeMessage |
                   UnidleMessage |
                   UnlockMessage |
                   UploadMessage |
                   EnterMessage
                   deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON MessageType where
  parseJSON (String v) = case unpack v of
                           "TextMessage"           -> pure TextMessage
                           "PasteMessage"          -> pure PasteMessage
                           "SoundMessage"          -> pure SoundMessage
                           "AdvertisementMessage"  -> pure AdvertisementMessage
                           "AllowGuestsMessage"    -> pure AllowGuestsMessage
                           "DisallowGuestsMessage" -> pure DisallowGuestsMessage
                           "IdleMessage"           -> pure IdleMessage
                           "KickMessage"           -> pure KickMessage
                           "LeaveMessage"          -> pure LeaveMessage
                           "SystemMessage"         -> pure SystemMessage
                           "TimestampMessage"      -> pure TimestampMessage
                           "TopicChangeMessage"    -> pure TopicChangeMessage
                           "UnidleMessage"         -> pure UnidleMessage
                           "UnlockMessage"         -> pure UnlockMessage
                           "UploadMessage"         -> pure UploadMessage
                           "EnterMessage"          -> pure EnterMessage
                           _                       -> mzero
  parseJSON _          = mzero

---------- Statements
-- |Statements are messages that you can send to CampFire.
data Statement = TextStatement { statementBody :: T.Text } |
                 PasteStatement { statementBody :: T.Text} |
                 SoundStatement { soundType :: Sound     } | -- ^ Play an audio message in the room
                 TweetStatement { statementUrl  :: T.Text}   -- ^ Display a tweet from a url on Twitter
                 deriving (Eq, Ord, Read, Show, Typeable)

instance ToJSON Statement where
  toJSON TextStatement  { statementBody = b} =
    "message" |- object ["type" .= ("TextMessage" :: T.Text), "body" .= b]
  toJSON PasteStatement { statementBody = b} =
    "message" |- object ["type" .= ("PasteMessage" :: T.Text), "body" .= b]
  toJSON SoundStatement { soundType = t    } =
    "message" |- object ["type" .= ("SoundMessage" :: T.Text), "body" .= t]
  toJSON TweetStatement { statementUrl = u } =
    "message" |- object ["type" .= ("TweetStatemetn" :: T.Text), "body" .= u]

(|-) :: T.Text -> Value -> Value
(|-) label obj = object [label .= obj]


-- |Different pre-set sounds that can be played in a room.
data Sound = Rimshot |
             Crickets |
             Trombone
             deriving (Eq, Ord, Read, Show, Typeable)

instance ToJSON Sound where
  toJSON Rimshot  = String "rimshot"
  toJSON Crickets = String "crickets"
  toJSON Trombone = String "trombone"


---------- Users

-- |User which can be found in any number of rooms.
data User = User { userId           :: Id,
                   userName         :: T.Text,
                   userEmailAddress :: T.Text,
                   userAdmin        :: Bool,   -- ^ User has administrative privileges
                   userCreatedAt    :: CampfireTime,
                   userType         :: UserType } 
            deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON User where
  -- consider using an ADT for type
  parseJSON (Object v) = User <$> v .: "id"
                              <*> v .: "name"
                              <*> v .: "email_address"
                              <*> v .: "admin"
                              <*> v .: "created_at"
                              <*> v .: "type"
  parseJSON _          = mzero

-- |Utility type used for extracting a User from the root JSON object the Campfire API returns
newtype UserWithRoot = UserWithRoot { unRootUser :: User } deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON UserWithRoot where
  parseJSON (Object v) = UserWithRoot <$> v .: "user"
  parseJSON _          = mzero

-- |Different classes of users that can be found in chat.
data UserType = Member | 
                Guest
                deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON UserType where
  parseJSON (String v) = case unpack v of
                           "Member" -> pure Member
                           "Guest"  -> pure Guest
                           _        -> mzero
  parseJSON _          = mzero

---------- Uploads

-- |File upload in a room.
data Upload = Upload { uploadId          :: Id,
                       uploadName        :: T.Text,
                       uploadRoomId      :: Id,
                       uploadUserId      :: Id,
                       uploadByteSize    :: Integer,
                       uploadContentType :: T.Text,
                       uploadFullUrl     :: T.Text,
                       uploadCreatedAt   :: CampfireTime }
              deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON Upload  where
  -- consider using an ADT for type
  parseJSON (Object v) = Upload <$> v .: "id"
                                <*> v .: "name"
                                <*> v .: "room_id"
                                <*> v .: "user_id"
                                <*> v .: "byte_size"
                                <*> v .: "content_type"
                                <*> v .: "full_url"
                                <*> v .: "created_at"
  parseJSON _          = mzero

-- |Utility type used for extracting a Upload from the list returned by the Campfire API
newtype Uploads = Uploads { unUploads :: [Upload] } deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON Uploads where
  parseJSON (Object v) = Uploads <$> v .: "uploads"
  parseJSON _          = mzero

-- |Utility type used for extracting an Upload from the root JSON object the Campfire API returns
newtype UploadWithRoot = UploadWithRoot { unRootUpload :: Upload } deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON UploadWithRoot where
  parseJSON (Object v) = UploadWithRoot <$> v .: "upload"
  parseJSON _          = mzero

---------- General Purpose Types
type Id = Integer

-- |Utility type to normalize the non-standard date format that the Campfire API returns
newtype CampfireTime = CampfireTime { 
                         fromCampfireTime :: UTCTime 
                       } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance FromJSON CampfireTime where
  parseJSON (String t) = case parseTime defaultTimeLocale "%Y/%m/%d %T %z" (unpack t) of
                           Just d -> pure (CampfireTime d)
                           _      -> fail "Could not parse Campfire-formatted time"
  parseJSON v          = AT.typeMismatch "CampfireTime" v

-- Helpers
(.:|) :: (FromJSON a) => Object -> (Text, a) -> AT.Parser a
obj .:| (key, d) = case M.lookup key obj of
                        Nothing -> pure d
                        Just v  -> parseJSON v

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Web.Campfire.Types where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Attoparsec (parse, maybeResult, eitherResult, Parser(..))
import Data.ByteString as BS (readFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Data.Map as M
import Data.Text
import Data.Typeable
import Locale (defaultTimeLocale)

---------- Rooms
data Room = Room { roomId               :: Id,
                   roomName             :: T.Text,
                   roomTopic            :: Maybe T.Text,
                   roomMembershipLimit  :: Integer,
                   roomFull             :: Maybe Bool,
                   roomOpenToGuests     :: Maybe Bool,
                   roomActiveTokenValue :: Maybe T.Text,
                   roomUpdatedAt        :: CampfireTime,
                   roomCreatedAt        :: CampfireTime,
                   roomUsers            :: Maybe [User] } deriving (Show)

 -- There is probably a better way to do this
instance FromJSON Room where
  parseJSON (Object v) = Room <$> v .:  T.pack "id"
                              <*> v .:  T.pack "name"
                              <*> v .:  T.pack "topic"
                              <*> v .:  T.pack "membership_limit"
                              <*> v .:? T.pack "full"
                              <*> v .:? T.pack "open_to_guests"
                              <*> v .:? T.pack "active_token_value"
                              <*> v .:  T.pack "updated_at"
                              <*> v .:  T.pack "created_at"
                              <*> v .:? T.pack "users"
  parseJSON _ = mzero

newtype Rooms = Rooms { unRooms :: [Room] } deriving (Show)

instance FromJSON Rooms where
  parseJSON (Object v) = Rooms <$> v .: T.pack "rooms"
  parseJSON _          = mzero


---------- Messages
data Message = Message { messageId        :: Id,
                         messageBody      :: T.Text,
                         messageRoomId    :: Id,
                         messageUserId    :: T.Text,
                         messageCreatedAt :: CampfireTime,
                         messageType      :: MessageType }

instance FromJSON Message where
  -- consider using an ADT for type
  parseJSON (Object v) = Message <$> v .: T.pack "id"
                                 <*> v .: T.pack "body"
                                 <*> v .: T.pack "room_id"
                                 <*> v .: T.pack "user_id"
                                 <*> v .: T.pack "created_at"
                                 <*> v .: T.pack "type"
  parseJSON _          = mzero

newtype Messages = Messages { unMessages :: [Room] } deriving (Show)

instance FromJSON Messages where
  parseJSON (Object v) = Messages <$> v .: T.pack "messages"
  parseJSON _          = mzero


data MessageType = TextMessage |
                   PasteMessage |
                   SoundMessage |
                   AdvertisementMessage |
                   AllowGuestsMessage |
                   DisallowGuestsMessage |
                   IdleMessage |
                   KickMessage |
                   LeaveMessage |
                   SystemMessage |
                   TimestampMessage |
                   TopicChangeMessage |
                   UnidleMessage |
                   UnlockMessage |
                   UploadMessage
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
                           _                       -> mzero
  parseJSON _          = mzero


---------- Users
data User = User { userId           :: Id,
                   userName         :: T.Text,
                   userEmailAddress :: T.Text,
                   userAdmin        :: Bool,
                   userCreatedAt    :: CampfireTime,
                   userType         :: UserType } 
            deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON User where
  -- consider using an ADT for type
  parseJSON (Object v) = User <$> v .: T.pack "id"
                              <*> v .: T.pack "name"
                              <*> v .: T.pack "email_address"
                              <*> v .: T.pack "admin"
                              <*> v .: T.pack "created_at"
                              <*> v .: T.pack "type"
  parseJSON _          = mzero

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
data Upload = Upload { uploadId          :: Id,
                       uploadName        :: T.Text,
                       uploadRoomId      :: Id,
                       uploadUserId      :: Id,
                       uploadByte_size   :: Integer,
                       uploadContentType :: T.Text,
                       uploadFullUrl     :: T.Text,
                       uploadCreatedAt   :: CampfireTime }
              deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON Upload  where
  -- consider using an ADT for type
  parseJSON (Object v) = Upload <$> v .: T.pack "id"
                                <*> v .: T.pack "name"
                                <*> v .: T.pack "room_id"
                                <*> v .: T.pack "user_id"
                                <*> v .: T.pack "byte_size"
                                <*> v .: T.pack "content_type"
                                <*> v .: T.pack "full_url"
                                <*> v .: T.pack "created_at"
  parseJSON _          = mzero

---------- General Purpose Types
type Id = Integer

newtype CampfireTime = CampfireTime { 
                         fromCampfireTime :: UTCTime 
                       } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance FromJSON CampfireTime where
  parseJSON (String t) = case parseTime defaultTimeLocale "%Y/%m/%d %T %z" (unpack t) of
                           Just d -> pure (CampfireTime d)
                           _      -> fail "Could not parse Campfire-formatted time"
  parseJSON v          = typeMismatch "CampfireTime" v


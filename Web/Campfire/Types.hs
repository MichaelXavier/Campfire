module Web.Campfire.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Attoparsec (parse, maybeResult, eitherResult)
import Data.ByteString as BS (readFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Time.Clock (UTCTime)

data Room = Room { roomId               :: Integer,
                   roomName             :: T.Text,
                   roomTopic            :: Maybe T.Text,
                   roomMembershipLimit  :: Integer,
                   roomFull             :: Bool,
                   roomOpenToGuests     :: Bool,
                   roomActiveTokenValue :: T.Text,
                   roomUpdatedAt        :: UTCTime,
                   roomCreatedAt        :: UTCTime,
                   roomUsers            :: Maybe [User] } deriving (Show)

--FIXME: it seems like room has 2 different formats, depending if you're
--getting it from /rooms.json or room/id.json
instance FromJSON Room where
 -- there should be a better way to do this
  parseJSON (Object v) = Room <$> v .: T.pack "id"
                              <*> v .: T.pack "name"
                              <*> v .: T.pack "topic"
                              <*> v .: T.pack "membership_limit"
                              <*> v .: T.pack "full"
                              <*> v .: T.pack "open_to_guests"
                              <*> v .: T.pack "active_token_value"
                              <*> v .: T.pack "updated_at"
                              <*> v .: T.pack "created_at"
                              <*> v .:? T.pack "users"
  parseJSON _ = mzero

newtype Rooms = Rooms { unRooms :: [Room] } deriving (Show)

instance FromJSON Rooms where
  parseJSON (Object v) = Rooms <$> v .: T.pack "rooms"
  parseJSON _          = mzero

data Message = Message { messageId        :: Integer,
                         messageBody      :: T.Text,
                         messageRoomId    :: Integer,
                         messageUserId    :: T.Text,
                         messageCreatedAt :: UTCTime,
                         messageType      :: T.Text }

instance FromJSON Message where
  -- consider using an ADT for type
  parseJSON (Object v) = Message <$> v .: T.pack "id"
                                 <*> v .: T.pack "body"
                                 <*> v .: T.pack "room_id"
                                 <*> v .: T.pack "user_id"
                                 <*> v .: T.pack "created_at"
                                 <*> v .: T.pack "type"

newtype Messages = Messages { unMessages :: [Room] } deriving (Show)

instance FromJSON Messages where
  parseJSON (Object v) = Messages <$> v .: T.pack "messages"
  parseJSON _          = mzero

data User = User { userId           :: Integer,
                   userName         :: T.Text,
                   userEmailAddress :: T.Text,
                   userAdmin        :: Bool,
                   userCreatedAt    :: UTCTime,
                   userType         :: T.Text } deriving (Show)

instance FromJSON User where
  -- consider using an ADT for type
  parseJSON (Object v) = User <$> v .: T.pack "id"
                              <*> v .: T.pack "name"
                              <*> v .: T.pack "email_address"
                              <*> v .: T.pack "admin"
                              <*> v .: T.pack "created_at"
                              <*> v .: T.pack "type"

main :: IO ()
main = BS.readFile "rooms.json" >>= printJSON . parsed
       where parsed txt = eitherResult $ parse json txt
             printJSON (Right obj) = putStrLn $ show $ (fromJSON obj :: Result Rooms)
             printJSON (Left err)  = putStrLn $ "ERROR: " ++ (show err)

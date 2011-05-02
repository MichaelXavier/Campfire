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
                   roomTopic            :: T.Text,
                   roomMembershipLimit  :: Integer,
                   roomFull             :: Bool,
                   roomOpenToGuests     :: Bool,
                   roomActiveTokenValue :: T.Text,
                   roomUpdatedAt        :: UTCTime,
                   roomUsers            :: [User],
                   roomCreatedAt        :: UTCTime } deriving (Show)

instance FromJSON Room where
 -- there should be a better way to do this
  parseJSON (Object v) = Room <$> v .: T.pack "id"
                              <*> v .: T.pack "name"
                              <*> v .: T.pack "topic"
                              <*> v .: T.pack "membership-limit"
                              <*> v .: T.pack "full"
                              <*> v .: T.pack "open-to-guests"
                              <*> v .: T.pack "active-token-value"
                              <*> v .: T.pack "updated-at"
                              <*> v .: T.pack "created-at"
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
                                 <*> v .: T.pack "room-id"
                                 <*> v .: T.pack "user-id"
                                 <*> v .: T.pack "created-at"
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
                   userType         :: T.Text }

instance FromJSON User where
  -- consider using an ADT for type
  parseJSON (Object v) = User <$> v .: T.pack "id"
                              <*> v .: T.pack "name"
                              <*> v .: T.pack "email-address"
                              <*> v .: T.pack "admin"
                              <*> v .: T.pack "created-at"
                              <*> v .: T.pack "type"

main :: IO ()
main = BS.readFile "rooms.json" >>= printJSON . parsed
       where parsed txt = eitherResult $ parse json txt
             printJSON (Right obj) = putStrLn $ show $ (fromJSON obj :: Result Rooms)
             printJSON (Left err)  = putStrLn $ "ERROR: " ++ (show err)

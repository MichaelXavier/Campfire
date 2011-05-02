module Web.Campfire.Types where

import Data.Time.Clock (UTCTime)
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Attoparsec (parse, maybeResult, eitherResult)
import Data.ByteString as BS (readFile)

data Room = Room { roomId               :: Integer,
                   roomName             :: T.Text,
                   roomTopic            :: T.Text,
                   roomMembershipLimit  :: Integer,
                   roomFull             :: Bool,
                   roomOpenToGuests     :: Bool,
                   roomActiveTokenValue :: T.Text,
                   roomUpdatedAt        :: UTCTime,
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
                   

--parseRooms :: BS.ByteString -> Either String [Room]
--parseRooms = eitherResult $ parse json txt

main :: IO ()
main = BS.readFile "rooms.json" >>= printJSON . parsed
       where parsed txt = eitherResult $ parse json txt
             printJSON (Right obj) = putStrLn $ show $ (fromJSON obj :: Result Rooms)
             printJSON (Left err)  = putStrLn $ "ERROR: " ++ (show err)

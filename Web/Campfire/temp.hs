{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.Parser
import qualified Data.Text as T

main :: IO ()
main = do
  txt <- readFile "room.json"
  putStrLn $ show $ ((fromJSON $ String $ T.pack txt) :: Result Object)

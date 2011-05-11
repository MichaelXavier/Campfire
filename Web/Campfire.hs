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
                      getMe,
                      getUser,
                      speak
                    ) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import Web.Campfire.Types
import Web.Campfire.Monad

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.Curl
import Network.Curl.Types
import Network.Curl.Code
import Data.Aeson
import Data.Attoparsec (parse, maybeResult, eitherResult)

--------- Room Operations
getRooms :: CampfireM [Room]
getRooms = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  resp <- doGet key sub "/rooms.json"
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result

getRoom :: Integer -> CampfireM Room
getRoom id = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  resp <- doGet key sub $ T.concat ["/room/", T.pack $ show id, ".json"]
  let result = handleResponse resp
  let room = unRootRoom $ (unWrap . readResult) result
  return $ room { roomId = id }

getPresence :: CampfireM [Room]
getPresence = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  resp <- doGet key sub "/presence.json"
  let result = handleResponse resp
  return $ (unRooms . unWrap . readResult) result


--------- User Operations
getMe :: CampfireM User
getMe = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  resp <- doGet key sub "/users/me.json"
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result

getUser :: Integer -> CampfireM User
getUser id = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  resp <- doGet key sub path
  let result = handleResponse resp
  return $ (unRootUser . unWrap . readResult) result
            where path = T.concat ["/users/", T.pack $ show id, ".json"]

--------- Message Operations
speak :: Integer -> Statement -> CampfireM (CurlCode, String)
speak roomId stmt = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  doPost key sub path stmt -- I think this is antipattern
            where path    = T.concat ["/rooms/", T.pack $ show roomId, ".json"]


--------- Helpers
unWrap :: (FromJSON a) => Result a -> a
unWrap (Success a) = a
unWrap (Error err) = error $ "parse error: " ++ err

doGet :: T.Text -> T.Text -> T.Text -> CampfireM (CurlCode, String)
doGet key sub path = liftIO $ curlGetString url opts
                     where url  = T.unpack $ cfURL path sub
                           opts = curlOpts key

--GRRAAAHHHHHHH DIE DIE DIE
doPost :: (ToJSON a) => T.Text -> T.Text -> T.Text -> a -> CampfireM (CurlCode, String)
doPost key sub path pay = liftIO $ curlGetString url opts
                     where opts    = method_POST ++ curlOpts key ++ [CurlVerbose True, post', post, ct, CurlFailOnError False, CurlHttpTransferDecoding False, CurlHttpContentDecoding False]
                           url     = T.unpack $ cfURL path sub
                           post'    = CurlPostFields [T.unpack encPay]
                           post    = CurlHttpPost [hpost]
                           hpost   = HttpPost { postName = "", contentType = Just "application/json", content = ContentString (T.unpack encPay), extraHeaders = [], showName = Nothing }
                           ct      = CurlHttpHeaders ["Content-Type: application/json"]
                           --encPath = encodePath path encPay
                           encPay  = encodePayload pay

encodePayload :: (ToJSON a) => a -> T.Text
encodePayload pay = T.pack $ LBS.unpack $ encode pay

handleResponse :: (CurlCode, String) -> Either CurlCode T.Text
handleResponse (CurlOK, str) = Right $ T.pack str
handleResponse (code, _)     = Left code

cfURL :: T.Text -> T.Text -> T.Text
cfURL path sub = T.concat ["https://", sub, ".campfirenow.com", path]

curlOpts :: T.Text -> [CurlOption]
curlOpts key = [CurlUserPwd $ T.unpack key, CurlPort 443]

--TODO proper error handling
readResult :: (FromJSON r) => Either CurlCode T.Text -> Result r
readResult (Right txt) = handleParse $ eitherResult $ parse json bs
                         where handleParse (Right obj) = fromJSON obj
                               handleParse (Left err) = error $ "Failed to parse: " ++ show err
                               bs = encodeUtf8 txt
readResult (Left code) = error $ "Unexpected Code: " ++ show code

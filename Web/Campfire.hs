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
module Web.Campfire ( getRooms ) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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

getRooms :: CampfireM [Room]
getRooms = do
  key <- asks cfKey
  sub <- asks cfSubDomain
  resp <- doGet key sub "/rooms.json"
  let result = handleResponse resp
  return $ unWrap $ readResult result
           where unWrap (Success rs) = unRooms rs
                 unWrap (Error err) = error $ "parse error: " ++ err

doGet :: T.Text -> T.Text -> T.Text -> CampfireM (CurlCode, String)
doGet key sub path = liftIO $ curlGetString url opts
                     where url  = T.unpack $ cfURL path sub
                           opts = curlOpts key

handleResponse :: (CurlCode, String) -> Either CurlCode T.Text
handleResponse (CurlOK, str) = Right $ T.pack str
handleResponse (code, _)     = Left code

cfURL :: T.Text -> T.Text -> T.Text
cfURL path sub = foldl1 T.append ["https://", sub, ".campfirenow.com", path]

curlOpts :: T.Text -> [CurlOption]
curlOpts key = [CurlUserPwd $ T.unpack key, CurlPort 443]

--TODO proper error handling
readResult :: (FromJSON r) => Either CurlCode T.Text -> Result r
readResult (Right txt) = handleParse $ eitherResult $ parse json bs
                         where handleParse (Right obj) = fromJSON obj
                               handleParse (Left err) = error $ "Failed to parse: " ++ show err
                               bs = encodeUtf8 txt
readResult (Left code) = error $ "Unexpected Code: " ++ show code

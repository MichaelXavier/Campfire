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

import Network.Curl
import Network.Curl.Types
import Network.Curl.Code
import Data.Aeson
import Data.Attoparsec (parse, maybeResult, eitherResult)

-- TODO: figure out authentication monad, types
getRooms :: CampfireEnv -> CampfireM [Room]
getRooms env =  doGet "/rooms.json" env >>= unRooms . (readResult :: Rooms)


doGet :: T.Text -> CampfireEnv -> CampfireM (Either CurlCode T.Text)
doGet path CampfireEnv { cfKey = key, 
                         cfSubDomain = sub} = do
                                                resp <- curlGetString url opts
                                                return $ handleResponse resp
                                              where url = T.unpack $ cfURL path sub
                                                    opts = curlOpts key

handleResponse :: (CurlCode, String) -> Either CurlCode T.Text
handleResponse (CurlOK, str) = Right $ T.pack str
handleResponse (code, _)     = Left code

cfURL :: T.Text -> T.Text -> T.Text
cfURL path sub = foldl1 T.append ["https://", sub, ".campfirenow.com", path]

curlOpts :: T.Text -> [CurlOption]
curlOpts key = [CurlUserPwd $ T.unpack key, CurlPort 443]

--TODO proper error handling
readResult :: (FromJSON r) => Either CurlCode T.Text -> r
readResult (Right txt) = handleParse $ eitherResult $ parse json bs
                         where handleParse (Right obj) = fromJSON obj
                               handleParse (Left err) = error $ "Failed to parse: " ++ (show err)
                               bs = encodeUtf8 txt
readResult (Left code) = error "Unexpected Code: " ++ (show code)

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

module Web.Campfire ( getRooms ) where

import qualified Data.Text as T
import Web.Campfire.Types
import Web.Campfire.Monad

-- TODO: figure out authentication monad, types
getRooms :: CampfireEnv -> CampfireM [Room]
getRooms env =  doGet "/rooms.json" env >>= readResult "results"


doGet :: T.Text -> CampfireEnv -> IO T.Text
doGet path env = undefined

readResult = undefined

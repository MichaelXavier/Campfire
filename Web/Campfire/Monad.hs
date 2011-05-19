--------------------------------------------------------------------
-- |
-- Module      : Web.Campfire.Monad
-- Description : Monadic interface for communcating with the Campfire API
-- Copyright   : (c) Michael Xavier 2011
-- License     : MIT
--
-- Maintainer: Michael Xavier <michael@michaelxavier.net>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.Campfire.Monad where

import qualified Data.Text as T
import Control.Monad.Reader

-- |Authentication environment used in Campfire API calls
data CampfireEnv = CampfireEnv { cfKey :: T.Text,      -- ^ Authentication token. Obtained from the user info page in Campfire
                                 cfSubDomain :: T.Text -- ^ The subdomain on the Campfire website to authenticate against
                               }

-- |IO wrapper used to chain and compose Campfire API actions
newtype CampfireM a = CampfireM {unCampfireM :: ReaderT CampfireEnv IO a} 
  deriving (Monad, MonadIO, MonadReader CampfireEnv)

{-# LANGUAGE OverloadedStrings #-}
module Web.Campfire.Monad where

import qualified Data.Text as T
import Control.Monad.Reader

-- There may be more here
data CampfireEnv = CampfireEnv { cfKey :: T.Text,
                                 cfSubDomain :: T.Text }

--FIXME: Need to see if I can/should make CampfireM a MonadIO instance instead
newtype CampfireM a = CampfireM {unCampfireM :: ReaderT CampfireEnv IO a}
--type CampfireM = IO

--instance Monad CampfireM where
--  -- Regardless of what env given, wrap an IO action
--  return x = CampfireM $ \_ -> return x
--  cfm >>= b = CampfireM $ \ cenv -> do
--                          result <- (unCampfireM cfm) cenv
--                          unCampfireM (b result) cenv


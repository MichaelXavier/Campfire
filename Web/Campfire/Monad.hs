{-# LANGUAGE OverloadedStrings #-}
module Web.Campfire.Monad where

import qualified Data.Text as T

-- There may be more here
data CampfireEnv = CampfireEnv { cfKey :: T.Text,
                                 cfSubDomain :: T.Text }

newtype CampfireM a = CampfireM {unCampfireM :: CampfireEnv -> IO a}

instance Monad CampfireM where
  -- Regardless of what env given, wrap an IO action
  return x = CampfireM $ \_ -> return x
  cfm >>= b = CampfireM $ \ cenv -> do
                          result <- (unCampfireM cfm) cenv
                          unCampfireM (b result) cenv


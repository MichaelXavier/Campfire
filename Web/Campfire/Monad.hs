{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.Campfire.Monad where

import qualified Data.Text as T
import Control.Monad.Reader

-- There may be more here
data CampfireEnv = CampfireEnv { cfKey :: T.Text,
                                 cfSubDomain :: T.Text }

newtype CampfireM a = CampfireM {unCampfireM :: ReaderT CampfireEnv IO a} 
  deriving (Monad, MonadIO, MonadReader CampfireEnv)

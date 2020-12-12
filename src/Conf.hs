{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Conf
  ( Conf(..)
  , cAWSCredentials
  , cAWSRegion
  , cCmd
  ) where

import           Control.Lens
import           Cmd
import qualified Network.AWS                   as AWS

data Conf = Conf
  { _cAWSCredentials :: AWS.Credentials -- ^ AWS credentials to use
  , _cAWSRegion      :: AWS.Region
  , _cCmd            :: AnyCmd
  }
  deriving Show

-- instance Default Conf where
--   def = Conf AWS.Discover AWS.Ireland def

makeLenses ''Conf


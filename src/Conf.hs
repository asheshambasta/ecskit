{-# LANGUAGE TemplateHaskell #-}
module Conf
  ( Conf(..)
  , cAWSCredentials
  , cAWSRegion
  , cCmd
  , Runtime(..)
  , rAWSEnv
  , rConf
  -- * Exec.
  , mkRuntime
  ) where

import           Control.Monad.Catch            ( MonadCatch )
import           Data.Default.Class             ( Default(..) )
import           Control.Lens
import           Cmd
import qualified Network.AWS                   as AWS

data Conf = Conf
  { _cAWSCredentials :: AWS.Credentials -- ^ AWS credentials to use
  , _cAWSRegion      :: AWS.Region
  , _cCmd            :: Cmd
  }
  deriving Show

instance Default Conf where
  def = Conf AWS.Discover AWS.Ireland def

makeLenses ''Conf

-- | Execution runtime, this is the container of the configuration at runtime.
data Runtime = Runtime
  { _rConf   :: Conf
  , _rAWSEnv :: AWS.Env
  }

makeLenses ''Runtime

-- | Create a new runtime for execution.
mkRuntime :: (MonadIO m, MonadCatch m) => Conf -> m Runtime
mkRuntime c@Conf {..} = Runtime c <$> AWS.newEnv _cAWSCredentials

instance AWS.HasEnv Runtime where
  environment = rAWSEnv

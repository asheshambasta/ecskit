{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , StrictData
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
#-}
module Conf.Runtime
  ( Runtime(..)
  , rAWSEnv
  , rConf
  -- * Exec.
  , mkRuntime
  ) where

import           Polysemy
import           Polysemy.Reader
import           Control.Monad.Catch            ( MonadCatch )
import qualified Network.AWS                   as AWS
import qualified Network.AWS.Env               as Env
import           Control.Lens
import           Conf

-- | Execution runtime, this is the container of the configuration at runtime.
data Runtime = Runtime
  { _rConf   :: Conf
  , _rAWSEnv :: AWS.Env
  }

makeLenses ''Runtime

-- | Create a new runtime for execution.
mkRuntime :: (MonadIO m, MonadCatch m) => Conf -> m Runtime
mkRuntime c@Conf {..} = Runtime c <$> do
  env <- AWS.newEnv _cAWSCredentials
  pure $ env { Env._envRegion = _cAWSRegion }

instance AWS.HasEnv Runtime where
  environment = rAWSEnv

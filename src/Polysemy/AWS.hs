{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , StrictData
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
#-}
module Polysemy.AWS
  ( liftAWS
  , AWSError(..)
  ) where

import           Polysemy
import "prelude-polysemy" Prelude.Control.Error
                                               as Err
import           Polysemy.Reader
import qualified Network.AWS                   as AWS

-- | Lift an AWS operation into a Sem
liftAWS
  :: forall a r
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => AWS.AWS a
  -> Sem r a
liftAWS aws = fromEitherM . catchErrors . runAWS =<< ask @AWS.Env
 where
  catchErrors = fmap (first AWSError) . try
  runAWS env = AWS.runResourceT . AWS.runAWS env $ aws

newtype AWSError = AWSError SomeException
                 deriving Show

instance Err.IsKnownError AWSError where
  errCode AWSError{} = "ERR.AWS.SOME_EXCEPTION"
  userMessage = Just . show
  errorLogLevel _ = levelCritical
  httpStatus _ = internalServerError500


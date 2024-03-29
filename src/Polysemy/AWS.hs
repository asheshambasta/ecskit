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
  , collectAWSResponses
  ) where

import qualified Network.AWS                   as AWS
import           Polysemy
import           Polysemy.Reader
import "prelude-polysemy" Prelude.Control.Error
                                               as Err

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

data AWSError = AWSError SomeException
              | EcrInvalidRepoName Text
              deriving Show

instance Err.IsKnownError AWSError where
  errCode AWSError{}           = errCode' "AWS.SOME_EXCEPTION"
  errCode EcrInvalidRepoName{} = errCode' "ECR.INVALID_REPO_NAME"
  userMessage = Just . show
  errorLogLevel _ = levelCritical
  httpStatus _ = internalServerError500

errCode' :: ErrCode -> ErrCode
errCode' = mappend "ERR.AWS."

-- | Collect AWS responses for paged results.
collectAWSResponses
  :: forall a r
   . ( Members '[Reader AWS.Env , Embed IO , Error AWSError] r
     , AWS.AWSRequest a
     )
  => a -- ^ Initial req.
  -> (a -> Text -> a) -- ^ How to set the next page token.
  -> (AWS.Rs a -> Maybe Text) -- ^ How to get the next page token from the response
  -> Sem r [AWS.Rs a] -- ^ Collection of all responses received from AWS
collectAWSResponses init setToken getToken = do
  res <- liftAWS $ AWS.send init
  collect [res] (getToken res)
 where
  collect acc = \case
    Nothing -> pure acc
    Just t ->
      let newReq = setToken init t
      in  do
            res <- liftAWS $ AWS.send newReq
            collect (acc <> [res]) (getToken res)

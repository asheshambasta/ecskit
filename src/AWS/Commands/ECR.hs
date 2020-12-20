{-# LANGUAGE
    TemplateHaskell
  , TypeApplications
  , TypeOperators
  , PolyKinds
  , TupleSections
  , StandaloneDeriving
#-}
module AWS.Commands.ECR
  ( EcrCmd(..)
  , ecrDescribeImages
  , ecrDescribeTaskDefImages
  , runEcrCmd
  , module ECR
  ) where

import           Control.Lens
import qualified Data.Map                      as M

import "this"    AWS.Types
import qualified Network.AWS                   as AWS
import           Network.AWS.ECR               as ECR
import           Network.AWS.ECS               as ECS

import           Polysemy
import           Polysemy.AWS
import           Polysemy.Reader

data EcrCmd m a where
  -- | Describe images in an ECR repository 
  EcrDescribeImages ::NonEmpty EcrRepoName -> EcrCmd m (Map EcrRepoName [ECR.ImageDetail])
  -- | Describe the `ECR.ImageDetail`'s used in all `ECS.ContainerDefinition`'s of an `ECS.TaskDefinition`. 
  -- Note: that a `ECS.TaskDefinition` can contain multiple `ECS.ContainerDefinition`'s. 
  EcrDescribeTaskDefImages ::ECS.TaskDefinition -> EcrCmd m (Map ContainerDefName [ECR.ImageDetail])

makeSem ''EcrCmd

runEcrCmd
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => Sem (EcrCmd ': r) a
  -> Sem r a
runEcrCmd = interpret $ \case
  EcrDescribeImages (toList -> names) ->
    M.fromList . zip names <$> mapM describeImages' names
  EcrDescribeTaskDefImages td ->
    let
      mImgs       = td ^. ECS.tdContainerDefinitions ^.. folded . ECS.cdImage
      mNames      = td ^. ECS.tdContainerDefinitions ^.. folded . ECS.cdName
      namesImages = bimap ContainerDefName EcrRepoName <$> catMaybes
        [ (,) <$> mName <*> mImg | (mName, mImg) <- zip mNames mImgs ]
      imgs = snd <$> namesImages
    in
      case nonEmpty imgs of
        Nothing    -> pure mempty
        Just imgs' -> do
          repNameImgs <- runEcrCmd . ecrDescribeImages $ imgs'
          pure . foldl' (addCdName repNameImgs) mempty $ namesImages
   where
    addCdName repNameImages acc (cdName', repoName) =
      case M.lookup repoName repNameImages of
        Nothing   -> acc
        Just dets -> M.insert cdName' dets acc

describeImages'
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => EcrRepoName
  -> Sem r [ECR.ImageDetail]
describeImages' (EcrRepoName rname) =
  let di = ECR.describeImages rname
  in  fmap (view ECR.dirsImageDetails) . liftAWS . AWS.send $ di

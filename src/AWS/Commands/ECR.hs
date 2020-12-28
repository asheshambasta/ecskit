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
import qualified Polysemy.Error                as Err
import           Polysemy.Reader

data EcrCmd m a where
  EcrParseImage ::Text -> EcrCmd m EcrImage
  -- | Describe images in an ECR repository 
  EcrDescribeImages ::[EcrRepoName] -> EcrCmd m (Map EcrRepoName [ECR.ImageDetail])
  -- | Describe the `ECR.ImageDetail`'s used in all `ECS.ContainerDefinition`'s of an `ECS.TaskDefinition`. 
  -- Note: that a `ECS.TaskDefinition` can contain multiple `ECS.ContainerDefinition`'s. 
  EcrDescribeTaskDefImages ::ECS.TaskDefinition -> EcrCmd m (Map ContainerDefName [ECR.ImageDetail])

makeSem ''EcrCmd

runEcrCmd
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => Sem (EcrCmd ': r) a
  -> Sem r a
runEcrCmd = interpret $ \case
  EcrDescribeImages names ->
    M.fromList . zip names <$> mapM describeImages' names
  EcrParseImage iT -> case ecrImage iT of
    Left  err -> Err.throw . EcrInvalidRepoName $ iT <> ": " <> show err
    Right i   -> pure i
  EcrDescribeTaskDefImages td -> do

    let mImgs  = td ^. ECS.tdContainerDefinitions ^.. folded . ECS.cdImage
        mNames = td ^. ECS.tdContainerDefinitions ^.. folded . ECS.cdName

    namesImages <- mapM parseEcrName $ catMaybes
      [ (,) <$> mName <*> mImg | (mName, mImg) <- zip mNames mImgs ]

    let imgs = snd <$> namesImages

    repNameImgs <- runEcrCmd . ecrDescribeImages $ imgs
    pure . foldl' (addCdName repNameImgs) mempty $ namesImages
   where
    parseEcrName (name, imgT) =
      (ContainerDefName name, ) . EcrRepoName <$> runEcrCmd (ecrParseImage imgT)
    addCdName repNameImages acc (cdName', repoName) =
      case M.lookup repoName repNameImages of
        Nothing   -> acc
        Just dets -> M.insert cdName' dets acc

describeImages'
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => EcrRepoName
  -> Sem r [ECR.ImageDetail]
describeImages' (unName -> EcrImage {..}) =
  let di = ECR.describeImages _ecriRepo & ECR.diRegistryId ?~ _ecriRegId
  in  fmap (view ECR.dirsImageDetails) . liftAWS . AWS.send $ di

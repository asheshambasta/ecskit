{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , StrictData
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
  , StandaloneDeriving
#-}
module Cmd
  ( Cmd(..)
  , AnyCmd(..)
  , UpdateMode(..)
  , describeServicesCmd
  , describeClustersCmd
  , listAllServicesCmd
  , listTaskDefsCmd
  , runCmd
  , runCmdExplicit
  , module AWS.Types
  ) where

import           Prelude                 hiding ( to )

import           Cmd.Results

import           Polysemy
import           Polysemy.Reader
import           Polysemy.Writer
import           Polysemy.AWS

import           Control.Lens

import "this"    AWS.Types

import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS.ListServices  as ECS
import qualified Network.AWS.ECS.Types         as ECS
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS

data AnyCmd where
  AnyCmd ::Cmd m a -> AnyCmd

deriving instance Show AnyCmd

data UpdateMode = Force | NoForce
                deriving (Eq, Show)

data Cmd m a where
  DescribeServicesCmd ::ClusterName -> NonEmpty ServiceName -> Cmd m [ServiceDescription]
  DescribeClustersCmd ::ECS.DescribeClusters -> Cmd m ECS.DescribeClustersResponse
  ListAllServicesCmd ::ClusterName -> Maybe ECS.LaunchType -> Cmd m [Arn 'AwsService]
  ListTaskDefsCmd ::TaskDefFamily -> Maybe ECS.TaskDefinitionStatus -> Cmd m [Arn 'AwsTaskDef]
  -- | Update the task definitions to the latest task def, or to a revision specified.
  -- 1. If no services are specified, all services of the given cluster are updated to the latest revisions of the TaskDefs.
  -- 2. Non-homogenous per-service task defs. are not supported, in these cases, we leave the service unchanged.
  -- 3. The `UpdateMode` is used to specify the AWS equivalent of @--force-new-update@
  UpdateTaskDefsCmd ::ClusterName
                    -> [(ServiceName, Maybe Int)]
                    -> UpdateMode
                    -> Cmd m [UpdateTaskDefResult]

makeSem ''Cmd
deriving instance Show (Cmd m a)

-- | Interpret an AWS Cmd.
runCmd
  :: forall a r
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Sem (Cmd ': r) a
  -> Sem r a
runCmd = interpret $ runCmdExplicit >=> pure . \case
  DescribeServicesResult svcs -> svcs
  DescribeClustersResult cs   -> cs
  ListServicesResult     svcs -> svcs
  ListTaskDefsResult     tds  -> tds

runCmdExplicit
  :: forall a r m
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Cmd m a
  -> Sem r (CmdResult a)
runCmdExplicit cmd = case cmd of
  DescribeServicesCmd cluster services -> do
    dsRes <- liftAWS . AWS.send $ ds
    let containers = dsRes ^. ECS.dssrsServices
        mkDesc container =
          let mtd = taskDefArn <$> container ^. ECS.csTaskDefinition
          in  ServiceDescription container
                <$> maybe (pure Nothing) (fmap Just . serviceTaskDef) mtd
    DescribeServicesResult <$> mapM mkDesc containers
   where
    ds =
      ECS.describeServices
        & (ECS.dCluster ?~ unName cluster)
        & (ECS.dServices .~ toList (unName <$> services))

  DescribeClustersCmd dcs -> DescribeClustersResult <$> liftAWS (AWS.send dcs)
  ListAllServicesCmd (Name cluster) lt ->
    ListServicesResult
      .   concatMap (fmap ServiceArn . view ECS.lsrsServiceARNs)
      <$> collectAWSResponses initReq
                              (\ls t -> ls & ECS.lsNextToken ?~ t)
                              (view ECS.lsrsNextToken)
   where
    initReq =
      ECS.listServices & ECS.lsCluster ?~ cluster & ECS.lsLaunchType .~ lt
  ListTaskDefsCmd f mStatus -> ListTaskDefsResult <$> listTaskDefs f mStatus

  UpdateTaskDefsCmd cluster svcRevs uMode -> fmap fst . runWriter $ do
    services <- runCmdExplicit $ ListAllServicesCmd cluster Nothing
    let invalidSvcs = if null givenSvcs then [] else undefined
    undefined
    where givenSvcs = fst <$> svcRevs


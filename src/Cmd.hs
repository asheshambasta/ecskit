{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , StrictData
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
#-}
module Cmd
  ( Cmd(..)
  , AnyCmd(..)
  , ClusterName(..)
  , describeServicesCmd
  , describeClustersCmd
  , listAllServicesCmd
  , listTaskDefsCmd
  , runCmd
  , runCmdExplicit
  ) where

import           Prelude                 hiding ( to )
import qualified GHC.Show

import           Cmd.Results
import           Cmd.Disp

import           Polysemy
import           Polysemy.Reader
import           Polysemy.AWS

import           Control.Lens

import           Data.List                      ( unwords )
import           Data.Default.Class             ( Default(..) )

import "this"    AWS.Types

import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS.ListServices  as ECS
import qualified Network.AWS.ECS.Types         as ECS
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS
import qualified Network.AWS.ECS.ListTaskDefinitions
                                               as ECS

data AnyCmd where
  AnyCmd ::Cmd m a -> AnyCmd

instance Show AnyCmd where
  show (AnyCmd cmd) = case cmd of
    DescribeServicesCmd ds  -> show ds
    DescribeClustersCmd dcs -> show dcs
    ListAllServicesCmd c mlt ->
      unwords ["ListAllServicesCmd", show c, show mlt]
    ListTaskDefsCmd prefix mStatus ->
      unwords ["ListTaskDefsCmd", show prefix, show mStatus]

instance Default AnyCmd where
  def = AnyCmd . DescribeServicesCmd $ ECS.describeServices

data Cmd m a where
  DescribeServicesCmd ::ECS.DescribeServices -> Cmd m [ServiceDescription]
  DescribeClustersCmd ::ECS.DescribeClusters -> Cmd m ECS.DescribeClustersResponse
  ListAllServicesCmd ::ClusterName -> Maybe ECS.LaunchType -> Cmd m [Arn 'ArnService]
  ListTaskDefsCmd ::TaskDefFamily -> Maybe ECS.TaskDefinitionStatus -> Cmd m [Arn 'ArnTaskDef]

makeSem ''Cmd

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
  DescribeServicesCmd ds -> do
    dsRes <- liftAWS . AWS.send $ ds
    let containers = dsRes ^. ECS.dssrsServices
        mkDesc container =
          let mtd = taskDefArn <$> container ^. ECS.csTaskDefinition
          in  ServiceDescription container
                <$> maybe (pure Nothing) (fmap Just . serviceTaskDef) mtd
    DescribeServicesResult <$> mapM mkDesc containers

  DescribeClustersCmd dcs -> DescribeClustersResult <$> liftAWS (AWS.send dcs)
  ListAllServicesCmd (ClusterName cluster) lt ->
    ListServicesResult
      .   concatMap (fmap ServiceArn . view ECS.lsrsServiceARNs)
      <$> collectAWSResponses initReq
                              (\ls t -> ls & ECS.lsNextToken ?~ t)
                              (view ECS.lsrsNextToken)
   where
    initReq =
      ECS.listServices & ECS.lsCluster ?~ cluster & ECS.lsLaunchType .~ lt
  ListTaskDefsCmd f mStatus -> ListTaskDefsResult <$> listTaskDefs f mStatus


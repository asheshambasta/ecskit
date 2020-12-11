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
  , ServiceARN(..)
  , describeServicesCmd
  , describeClustersCmd
  , listAllServicesCmd
  , listTaskDefsCmd
  , runCmd
  , runCmdExplicit
  ) where

import qualified GHC.Show

import           Cmd.Disp.ANSI.Helpers
import           Cmd.Disp

import           Polysemy
import           Polysemy.Reader
import           Polysemy.AWS

import           Control.Lens

import qualified Data.Text                     as T
import           Data.List                      ( unwords )
import           Data.Default.Class             ( Default(..) )

import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS.ListServices  as ECS
import qualified Network.AWS.ECS.Types         as ECS
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS
import qualified Network.AWS.ECS.ListTaskDefinitions
                                               as ECS

-- | Cluster name
newtype ClusterName = ClusterName { unClusterName :: Text}
                    deriving (Eq, Show, IsString) via Text

newtype ServiceARN = ServiceARN { unServiceARN :: Text }
                   deriving (Eq, Show, IsString) via Text

newtype TaskDefFamily = TaskDefFamily { unTaskDefFamily :: Text }
                      deriving (Eq, Show, IsString) via Text

newtype TaskDefARN = TaskDefARN { unTaskDefARN :: Text }
                   deriving (Eq, Show, IsString) via Text

instance Disp 'Terminal [ServiceARN] where
  disp arns =
    let arnsStr = "\n\t" <> T.intercalate "\n\t" (unServiceARN <$> arns)
    in  withAnsiReset . withStdColours $ propertyNameContent "Service ARNs"
                                                             (Just arnsStr)

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
  DescribeServicesCmd ::ECS.DescribeServices -> Cmd m ECS.DescribeServicesResponse
  DescribeClustersCmd ::ECS.DescribeClusters -> Cmd m ECS.DescribeClustersResponse
  ListAllServicesCmd ::ClusterName -> Maybe ECS.LaunchType -> Cmd m [ServiceARN]
  ListTaskDefsCmd ::TaskDefFamily -> Maybe ECS.TaskDefinitionStatus -> Cmd m [TaskDefARN]

makeSem ''Cmd

data CmdResult a where
  DescribeClustersResult ::ECS.DescribeClustersResponse -> CmdResult ECS.DescribeClustersResponse
  DescribeServicesResult ::ECS.DescribeServicesResponse -> CmdResult ECS.DescribeServicesResponse
  ListServicesResult ::[ServiceARN] -> CmdResult [ServiceARN]
  ListTaskDefsResult ::[TaskDefARN] -> CmdResult [TaskDefARN]

instance Disp 'Terminal (CmdResult a) where
  disp = \case
    DescribeClustersResult cRes ->
      mapM_ (disp @ 'Terminal) (cRes ^. ECS.dcrsClusters)
    ListServicesResult     arns -> (disp @ 'Terminal) arns
    DescribeServicesResult dsr  -> (disp @ 'Terminal) (dsr ^. ECS.dssrsServices)
    ListTaskDefsResult     tds  -> withAnsiReset . withStdColours $ do
      propertyName "TaskDefs"
      newline
      putStrLn $ "\n\t" <> T.intercalate "\n\t- " (unTaskDefARN <$> tds)

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
  DescribeServicesCmd ds  -> DescribeServicesResult <$> liftAWS (AWS.send ds)
  DescribeClustersCmd dcs -> DescribeClustersResult <$> liftAWS (AWS.send dcs)
  ListAllServicesCmd (ClusterName cluster) lt ->
    ListServicesResult
      .   concatMap (fmap ServiceARN . view ECS.lsrsServiceARNs)
      <$> collectAWSResponses initReq
                              (\ls t -> ls & ECS.lsNextToken ?~ t)
                              (view ECS.lsrsNextToken)
   where
    initReq =
      ECS.listServices & ECS.lsCluster ?~ cluster & ECS.lsLaunchType .~ lt
  ListTaskDefsCmd (TaskDefFamily prefix) mStatus ->
    ListTaskDefsResult
      .   concatMap (fmap TaskDefARN . view ECS.ltdrsTaskDefinitionARNs)
      <$> collectAWSResponses initReq
                              (\ltds t -> ltds & ECS.ltdNextToken ?~ t)
                              (view ECS.ltdrsNextToken)
   where
    initReq =
      ECS.listTaskDefinitions
        &  ECS.ltdStatus
        .~ mStatus
        &  ECS.ltdFamilyPrefix
        ?~ prefix


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

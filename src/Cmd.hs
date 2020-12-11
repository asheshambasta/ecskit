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
  , runCmd
  , runCmdExplicit
  ) where

import qualified GHC.Show

import           Cmd.Disp.ANSI.Helpers          ( propertyNameContent
                                                , withStdColours
                                                )
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

-- | Cluster name
newtype ClusterName = ClusterName { unClusterName :: Text}
                    deriving (Eq, Show, IsString) via Text

newtype ServiceARN = ServiceARN { unServiceARN :: Text }
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

instance Default AnyCmd where
  def = AnyCmd . DescribeServicesCmd $ ECS.describeServices

data Cmd m a where
  DescribeServicesCmd ::ECS.DescribeServices -> Cmd m ECS.DescribeServicesResponse
  DescribeClustersCmd ::ECS.DescribeClusters -> Cmd m ECS.DescribeClustersResponse
  ListAllServicesCmd ::ClusterName -> Maybe ECS.LaunchType -> Cmd m [ServiceARN]

makeSem ''Cmd

data CmdResult a where
  DescribeClustersResult ::ECS.DescribeClustersResponse -> CmdResult ECS.DescribeClustersResponse
  DescribeServicesResult ::ECS.DescribeServicesResponse -> CmdResult ECS.DescribeServicesResponse
  ListServicesResult ::[ServiceARN] -> CmdResult [ServiceARN]

instance Disp 'Terminal (CmdResult a) where
  disp = \case
    DescribeClustersResult cRes ->
      mapM_ (disp @ 'Terminal) (cRes ^. ECS.dcrsClusters)
    ListServicesResult     arns -> (disp @ 'Terminal) arns
    DescribeServicesResult dsr  -> (disp @ 'Terminal) (dsr ^. ECS.dssrsServices)

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

runCmdExplicit
  :: forall a r m
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Cmd m a
  -> Sem r (CmdResult a)
runCmdExplicit cmd = case cmd of
  DescribeServicesCmd ds -> DescribeServicesResult <$> liftAWS (AWS.send ds)
  DescribeClustersCmd dcs -> DescribeClustersResult <$> liftAWS (AWS.send dcs)
  ListAllServicesCmd (ClusterName cluster) lt -> ListServicesResult
    <$> liftAWS (collectServices Nothing mempty)
   where
    collectServices mNextPage acc =
      let ls =
            ECS.listServices
              & (ECS.lsCluster ?~ cluster)
              & (ECS.lsNextToken .~ mNextPage)
              & (ECS.lsLaunchType .~ lt)
      in  do
            res <- AWS.send ls
            let arns     = ServiceARN <$> res ^. ECS.lsrsServiceARNs
                nextPage = res ^. ECS.lsrsNextToken
                acc'     = acc <> arns
            if isJust nextPage then collectServices nextPage acc' else pure acc'


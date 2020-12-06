{-# LANGUAGE ApplicativeDo #-}
module Parse.Cmd
  ( cmdParse
  ) where

import           Control.Lens
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS
import           Cmd
import qualified Options.Applicative           as A

cmdParse :: A.Parser AnyCmd
cmdParse = A.subparser $ describeServicesCmds <> describeClustersCmds

describeServicesCmds :: A.Mod A.CommandFields AnyCmd
describeServicesCmds =
  A.command "ds" describeServices
    <> A.command "describe-services" describeServices

describeClustersCmds :: A.Mod A.CommandFields AnyCmd
describeClustersCmds =
  A.command "dc" describeClusters
    <> A.command "describe-clusters" describeClusters

describeServices :: A.ParserInfo AnyCmd
describeServices = AnyCmd
  <$> A.info describeOpts (A.progDesc "Describe services.")
 where
  describeOpts =
    mkDescribe
      <$> A.optional (A.strOption (A.long "cluster" <> A.short 'C'))
      <*> (A.many . A.strOption $ A.long "service" <> A.short 'S')
  mkDescribe c svcs =
    Cmd.DescribeServicesCmd
      $  ECS.describeServices
      &  ECS.dCluster
      .~ c
      &  ECS.dServices
      .~ svcs

describeClusters :: A.ParserInfo AnyCmd
describeClusters = AnyCmd
  <$> A.info describeOpts (A.progDesc "Describe clusters.")
 where
  describeOpts =
    mkDescribe <$> (A.many . A.strOption $ A.long "cluster" <> A.short 'C')
  mkDescribe clusters =
    Cmd.DescribeClustersCmd $ ECS.describeClusters & ECS.dcClusters .~ clusters

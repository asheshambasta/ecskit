module Parse.Cmd
  ( cmdParse
  ) where

import           Control.Lens
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS
import           Cmd

import qualified Options.Applicative.Types     as A
import qualified Options.Applicative           as A

-- | Parse all commands.
-- Each subgroup of commands constains a list of alternatives, one a short form for those who hate typing,
-- and another more verbose, for those who don't. 
cmdParse :: A.Parser AnyCmd
cmdParse =
  A.subparser
    $  altCmds ["c", "cluster"]          clusterCmds
    <> altCmds ["s", "service"]          serviceCmds
    <> altCmds ["td", "task-definition"] taskDefCmds
 where
  taskDefCmds =
    let parser = A.subparser $ A.command "ls" listTaskDefs
    in  A.info parser $ A.progDesc "Task definition commands."
  serviceCmds =
    let parser =
          A.subparser
            $  A.command "ls" listServices
            <> A.command "d" describeServices
    in  A.info parser $ A.progDesc "Service commands."
  clusterCmds =
    let parser =
          A.subparser
            $  A.command "d" describeClusters
            <> A.command "describe" describeClusters
    in  A.info parser $ A.progDesc "Cluster commands."
  altCmds names parser = mconcat [ A.command name parser | name <- names ]

listTaskDefs :: A.ParserInfo AnyCmd
listTaskDefs = AnyCmd <$> A.info listOpts (A.progDesc "List task definitions.")
 where
  listOpts =
    Cmd.ListTaskDefsCmd
      <$> A.strOption (A.long "prefix" <> A.short 'p')
      <*> A.optional
            (A.option (A.eitherReader readEither)
                      (A.long "status" <> A.short 's')
            )

listServices :: A.ParserInfo AnyCmd
listServices = AnyCmd
  <$> A.info listOpts (A.progDesc "List services in a cluster.")
 where
  listOpts =
    Cmd.ListAllServicesCmd
      <$> A.strOption (A.long "cluster" <> A.short 'C')
      <*> A.optional
            (A.option (A.eitherReader readEither) (A.long "launch-type"))

describeServices :: A.ParserInfo AnyCmd
describeServices = AnyCmd
  <$> A.info describeOpts (A.progDesc "Describe services in a cluster.")
 where
  describeOpts =
    mkDescribe
      <$> A.strOption (A.long "cluster" <> A.short 'C')
      <*> (many1 . A.strOption $ A.long "service" <> A.short 'S')
  mkDescribe c svcs =
    Cmd.DescribeServicesCmd
      $  ECS.describeServices
      &  ECS.dCluster
      ?~ c
      &  ECS.dServices
      .~ toList svcs

describeClusters :: A.ParserInfo AnyCmd
describeClusters = AnyCmd
  <$> A.info describeOpts (A.progDesc "Describe clusters.")
 where
  describeOpts =
    mkDescribe <$> (many1 . A.strOption $ A.long "cluster" <> A.short 'C')
  mkDescribe clusters =
    Cmd.DescribeClustersCmd
      $  ECS.describeClusters
      &  ECS.dcClusters
      .~ toList clusters

many1 :: A.Parser a -> A.Parser (NonEmpty a)
many1 p = A.fromM $ (:|) <$> A.oneM p <*> A.manyM p

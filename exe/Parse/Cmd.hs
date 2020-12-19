module Parse.Cmd
  ( cmdParse
  ) where

import           Cmd
import           Control.Lens
import           Data.Default.Class             ( def )
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS

import qualified Options.Applicative           as A
import qualified Options.Applicative.Types     as A

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
    let
      parser =
        A.subparser
          $  A.command "ls" listServices
          <> A.command "d" describeServices
          <> altCmds ["u", "update-task-definition"] updateTaskDef
          <> altCmds ["dutd", "describe-used-task-definition"]
                     describeUsedTaskDef
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
      <$> A.strOption
            (A.long "cluster" <> A.short 'C' <> A.help "Name of the cluster.")
      <*> A.optional
            (A.option (A.eitherReader readEither) (A.long "launch-type"))

clusterName :: A.Parser ClusterName
clusterName =
  A.strOption (A.long "cluster" <> A.short 'C' <> A.help "Name of the cluster.")

serviceName :: A.Parser ServiceName
serviceName =
  A.strOption (A.long "service" <> A.short 'S' <> A.help "Name of the service.")

-- | Update the task def; if no specific revision is provided, the latest revision is used.
-- This only handles continuous task defintions, or task defintion names with some notion of "ordering".
updateTaskDef :: A.ParserInfo AnyCmd
updateTaskDef = AnyCmd <$> A.info updateOpts (A.progDesc "Update service.")
 where
  updateOpts =
    Cmd.UpdateTaskDefsCmd
      <$> clusterName
      <*> many serviceNameRevision
      <*> A.optional modeOpts
  serviceNameRevision = (,) <$> serviceName <*> A.optional
    (A.option A.auto $ A.long "revision" <> A.short 'R' <> A.help
      "Revision to use."
    )
  modeOpts = A.option
    (A.eitherReader readEither)
    (A.long "mode" <> A.short 'M' <> A.help help <> A.value def <> A.showDefault
    )
  help = "Mode of update; use one of: "
    <> intercalate ", " (show <$> enumFromTo @UpdateMode minBound maxBound)

describeUsedTaskDef :: A.ParserInfo AnyCmd
describeUsedTaskDef = AnyCmd
  <$> A.info opts (A.progDesc "Describe used task definitions")
  where opts = DescribeUsedTaskDefsCmd <$> clusterName <*> many1 serviceName

describeServices :: A.ParserInfo AnyCmd
describeServices = AnyCmd
  <$> A.info describeOpts (A.progDesc "Describe services in a cluster.")
 where
  describeOpts =
    Cmd.DescribeServicesCmd
      <$> A.strOption (A.long "cluster" <> A.short 'C')
      <*> (many1 . A.strOption $ A.long "service" <> A.short 'S')

describeClusters :: A.ParserInfo AnyCmd
describeClusters = AnyCmd
  <$> A.info describeOpts (A.progDesc "Describe clusters.")
 where
  describeOpts =
    mkDescribe <$> (many1 . A.strOption $ A.long "cluster" <> A.short 'C')
  mkDescribe clusters =
    Cmd.DescribeClustersCmd
      $ ECS.describeClusters
      & (ECS.dcClusters .~ toList clusters)

many1 :: A.Parser a -> A.Parser (NonEmpty a)
many1 p = A.fromM $ (:|) <$> A.oneM p <*> A.manyM p

{-# LANGUAGE ApplicativeDo #-}
module Parse.Cmd
  ( cmdParse
  ) where

import           Control.Lens
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS
import           Cmd
import qualified Options.Applicative           as A

cmdParse :: A.Parser Cmd
cmdParse = A.subparser describeServicesCmds

describeServicesCmds :: A.Mod A.CommandFields Cmd
describeServicesCmds =
  A.command "ds" describeServices
    <> A.command "describe-services" describeServices

describeServices :: A.ParserInfo Cmd
describeServices = A.info describeOpts (A.progDesc "Describe services.")
 where
  describeOpts =
    mkDescribe
      <$> (A.optional $ A.strOption (A.long "cluster" <> A.short 'C'))
      <*> (A.many . A.strOption $ A.long "service" <> A.short 'S')
  mkDescribe c svcs =
    Cmd.DescribeServices
      $  ECS.describeServices
      &  ECS.dCluster
      .~ c
      &  ECS.dServices
      .~ svcs

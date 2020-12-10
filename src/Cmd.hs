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
  , describeServicesCmd
  , describeClustersCmd
  , runCmd
  , runCmdExplicit
  ) where

import qualified GHC.Show

import           Cmd.Disp

import           Polysemy
import           Polysemy.Reader
import           Polysemy.AWS

import           Control.Lens

import           Data.Default.Class             ( Default(..) )

import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS

data AnyCmd where
  AnyCmd ::Cmd m a -> AnyCmd

instance Show AnyCmd where
  show (AnyCmd cmd) = case cmd of
    DescribeServicesCmd ds  -> show ds
    DescribeClustersCmd dcs -> show dcs

instance Default AnyCmd where
  def = AnyCmd . DescribeServicesCmd $ ECS.describeServices

data Cmd m a where
  DescribeServicesCmd ::ECS.DescribeServices -> Cmd m ECS.DescribeServicesResponse
  DescribeClustersCmd ::ECS.DescribeClusters -> Cmd m ECS.DescribeClustersResponse

makeSem ''Cmd

data CmdResult = DescribeServicesResult ECS.DescribeServicesResponse
               | DescribeClustersResult ECS.DescribeClustersResponse
               deriving Show

instance Disp 'Terminal CmdResult where
  disp = \case
    DescribeClustersResult cRes ->
      mapM_ (disp @ 'Terminal) (cRes ^. ECS.dcrsClusters)

-- | Interpret an AWS Cmd.
runCmd
  :: forall a r
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Sem (Cmd ': r) a
  -> Sem r a
runCmd = interpret $ \case
  DescribeServicesCmd ds -> liftAWS $ AWS.send ds
  DescribeClustersCmd dc -> liftAWS $ AWS.send dc

runCmdExplicit
  :: forall a r m
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Cmd m a
  -> Sem r CmdResult
runCmdExplicit cmd = case cmd of
  DescribeServicesCmd ds  -> DescribeServicesResult <$> liftAWS (AWS.send ds)
  DescribeClustersCmd dcs -> DescribeClustersResult <$> liftAWS (AWS.send dcs)


{-|
Module: Cmd.Results
Description: Specific result types.
-}
module Cmd.Results
  ( ServiceDescription(..)
  , UpdateTaskDefResult(..)
  -- * The all encompassing result type
  , CmdResult(..)
  ) where

import           Control.Lens

import           Cmd.Disp
import           Cmd.Disp.ANSI.Helpers

import "this"    AWS.Types
import qualified Network.AWS.ECS               as ECS

data ServiceDescription = ServiceDescription ECS.ContainerService
                                             (Maybe ServiceTaskDef)
  deriving (Show, Eq)

instance Disp 'Terminal ServiceDescription where
  disp (ServiceDescription cs mTd) =
    withAnsiReset
      .  withStdColours
      $  disp @ 'Terminal cs
      >> maybe noTaskDef (disp @ 'Terminal) mTd
    where noTaskDef = putStrLn @Text "No TaskDef data found."

-- | Result of updating the task definition of a service
data UpdateTaskDefResult = UpdateTdFailed ServiceName Text
                         -- | On success, we get the new `ServiceDescription`
                         | UpdateTdSuccess ServiceName ServiceDescription
                         deriving (Show, Eq)

data CmdResult a where
  DescribeClustersResult ::ECS.DescribeClustersResponse -> CmdResult ECS.DescribeClustersResponse
  DescribeServicesResult ::[ServiceDescription] -> CmdResult [ServiceDescription]
  ListServicesResult ::[Arn 'AwsService] -> CmdResult [Arn 'AwsService]
  ListTaskDefsResult ::[Arn 'AwsTaskDef] -> CmdResult [Arn 'AwsTaskDef]
  UpdateTaskDefsResult ::[UpdateTaskDefResult] -> CmdResult [UpdateTaskDefResult]

instance Disp 'Terminal (CmdResult a) where
  disp = \case
    DescribeClustersResult cRes ->
      mapM_ (disp @ 'Terminal) (cRes ^. ECS.dcrsClusters)
    ListServicesResult     arns    -> (disp @ 'Terminal) arns
    DescribeServicesResult descs   -> mapM_ (disp @ 'Terminal) descs
      -- (disp @ 'Terminal) (dsr ^. ECS.dssrsServices)
    ListTaskDefsResult     tds     -> (disp @ 'Terminal) tds
    UpdateTaskDefsResult   results -> mapM_ (disp @ 'Terminal) results

instance Disp 'Terminal UpdateTaskDefResult where

instance Semigroup (CmdResult [UpdateTaskDefResult]) where
  (UpdateTaskDefsResult rs0) <> (UpdateTaskDefsResult rs1) =
    UpdateTaskDefsResult $ rs0 <> rs1
instance Monoid (CmdResult [UpdateTaskDefResult]) where
  mempty = UpdateTaskDefsResult mempty

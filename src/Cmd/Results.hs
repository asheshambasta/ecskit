{-# LANGUAGE TemplateHaskell #-}
{-|
Module: Cmd.Results
Description: Specific result types.
-}
module Cmd.Results
  ( ServiceDescription(..)
  , sdContainerService
  , sdServiceTaskDef
  , ServiceResult(..)
  , UpdateTaskDefResult
  -- * The all encompassing result type
  , CmdResult(..)
  ) where

import           Control.Lens

import           Cmd.Disp
import           Cmd.Disp.ANSI.Helpers

import "this"    AWS.Types
import qualified Network.AWS.ECS               as ECS

data ServiceDescription = ServiceDescription
  { _sdContainerService :: ECS.ContainerService
  , _sdServiceTaskDef   :: Maybe ServiceTaskDef
  }
  deriving (Show, Eq)

makeLenses ''ServiceDescription

instance Disp 'Terminal ServiceDescription where
  disp (ServiceDescription cs mTd) =
    withAnsiReset
      .  withStdColours
      $  disp @ 'Terminal cs
      >> maybe noTaskDef (disp @ 'Terminal) mTd
    where noTaskDef = putStrLn @Text "No TaskDef data found."

-- | A Multiple service operation; which can result in success or failures by service.
data ServiceResult failure success info = ServiceFailed ServiceName failure
                                        | ServiceSuccess ServiceName success
                                        | ServiceInfo ServiceName info
                                        | ServiceWarn ServiceName failure
                                        deriving (Eq, Show)

-- | Result of updating the task definition of a service
type UpdateTaskDefResult = ServiceResult Text ServiceDescription Text

data CmdResult a where
  DescribeClustersResult ::ECS.DescribeClustersResponse -> CmdResult ECS.DescribeClustersResponse
  DescribeServicesResult ::[ServiceDescription] -> CmdResult [ServiceDescription]
  ListServicesResult ::[Arn 'AwsService] -> CmdResult [Arn 'AwsService]
  ListTaskDefsResult ::[Arn 'AwsTaskDef] -> CmdResult [Arn 'AwsTaskDef]
  UpdateTaskDefsResult ::[UpdateTaskDefResult] -> CmdResult [UpdateTaskDefResult]
  DescribeUsedTaskDefsResult ::[(ServiceName, Maybe ECS.TaskDefinition)] -> CmdResult [(ServiceName, Maybe ECS.TaskDefinition)]

instance Disp 'Terminal (CmdResult a) where
  disp = \case
    DescribeClustersResult cRes ->
      mapM_ (disp @ 'Terminal) (cRes ^. ECS.dcrsClusters)
    ListServicesResult         arns    -> (disp @ 'Terminal) arns
    DescribeServicesResult     descs   -> mapM_ (disp @ 'Terminal) descs
      -- (disp @ 'Terminal) (dsr ^. ECS.dssrsServices)
    ListTaskDefsResult         tds     -> (disp @ 'Terminal) tds
    UpdateTaskDefsResult       results -> mapM_ (disp @ 'Terminal) results
    DescribeUsedTaskDefsResult results -> mapM_ dispResult results
     where
      dispResult (s, mtd) =
        withAnsiReset
          .  withStdColours
          $  disp @ 'Terminal s
          >> disp @ 'Terminal mtd

instance Disp 'Terminal UpdateTaskDefResult where
  disp = withAnsiReset . withStdColours . \case

    ServiceFailed name (mappend "[FAILED] " -> msg) -> do
      disp @ 'Terminal name
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ indentedNoLeadingNewline identity msg

    ServiceInfo name (mappend "[INFO] " -> info') -> do
      disp @ 'Terminal name
      setSGR [SetColor Foreground Vivid Magenta]
      putStrLn $ indentedNoLeadingNewline identity info'

    ServiceSuccess name desc -> do
      disp @ 'Terminal name
      setSGR [SetColor Foreground Vivid Green]
      putStrLn @Text "[SUCCESS]"
      disp @ 'Terminal desc

    ServiceWarn name (mappend "[WARNING] " -> warning) -> do
      disp @ 'Terminal name
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn warning

instance Semigroup (CmdResult [UpdateTaskDefResult]) where
  (UpdateTaskDefsResult rs0) <> (UpdateTaskDefsResult rs1) =
    UpdateTaskDefsResult $ rs0 <> rs1
instance Monoid (CmdResult [UpdateTaskDefResult]) where
  mempty = UpdateTaskDefsResult mempty

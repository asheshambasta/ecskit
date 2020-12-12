{-|
Module: Cmd.Results
Description: Specific result types.
-}
module Cmd.Results
  ( ServiceDescription(..)
  , CmdResult(..)
  ) where

import           Control.Lens

import           Cmd.Disp
import           Cmd.Disp.ANSI.Helpers

import "this"    AWS.Types
import qualified Network.AWS.ECS               as ECS

data ServiceDescription = ServiceDescription ECS.ContainerService
                                             (Maybe ServiceTaskDef)

instance Disp 'Terminal ServiceDescription where
  disp (ServiceDescription cs mTd) =
    withAnsiReset
      .  withStdColours
      $  disp @ 'Terminal cs
      >> maybe noTaskDef (disp @ 'Terminal) mTd
    where noTaskDef = putStrLn @Text "No TaskDef data found."

data CmdResult a where
  DescribeClustersResult ::ECS.DescribeClustersResponse -> CmdResult ECS.DescribeClustersResponse
  DescribeServicesResult ::[ServiceDescription] -> CmdResult [ServiceDescription]
  ListServicesResult ::[Arn 'ArnService] -> CmdResult [Arn 'ArnService]
  ListTaskDefsResult ::[Arn 'ArnTaskDef] -> CmdResult [Arn 'ArnTaskDef]

instance Disp 'Terminal (CmdResult a) where
  disp = \case
    DescribeClustersResult cRes ->
      mapM_ (disp @ 'Terminal) (cRes ^. ECS.dcrsClusters)
    ListServicesResult     arns  -> (disp @ 'Terminal) arns
    DescribeServicesResult descs -> mapM_ (disp @ 'Terminal) descs
      -- (disp @ 'Terminal) (dsr ^. ECS.dssrsServices)
    ListTaskDefsResult     tds   -> (disp @ 'Terminal) tds

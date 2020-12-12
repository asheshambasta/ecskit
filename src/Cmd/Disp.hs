{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: Cmd.Encode 
Description: Disp on various `DispMedium`.
-}
module Cmd.Disp
  ( DispResult
  , DispMedium(..)
  , Disp(..)
  , withAnsiReset
  ) where

import           Cmd.Disp.ANSI.Helpers
import           Control.Lens
import qualified Network.AWS.ECS.Types         as ECS

import qualified Data.Text                     as T
import           Data.Aeson

data DispMedium = Terminal | Json
                   deriving (Eq, Show)

-- | Disp something in the terminal, the result of displaying the value in the terminal
class Disp (medium :: DispMedium) a where

  {-# MINIMAL disp #-}

  -- | Disp a value in the terminal
  disp :: a -> DispResult medium

-- | The result of displaying on some `DispMedium` 
type family DispResult (m :: DispMedium) where
  DispResult 'Terminal = IO ()
  DispResult 'Json = Value

instance Disp 'Terminal ECS.Cluster where
  disp c = withAnsiReset . withStdColours $ do
    propertyNameContent "Name" $ c ^. ECS.cClusterName
    propertyNameContent "ARN" $ c ^. ECS.cClusterARN
    propertyNameContent "Status" $ c ^. ECS.cStatus
    propertyNameContent "Active services"
                        (show @Int @Text <$> c ^. ECS.cActiveServicesCount)
    propertyNameContent "Running tasks"
      $   show @Int @Text
      <$> c
      ^.  ECS.cRunningTasksCount

instance Disp 'Terminal [ECS.ContainerService] where
  disp = withAnsiReset . withStdColours . mapM_ (disp @ 'Terminal)

instance Disp 'Terminal ECS.ContainerService where
  disp cs = withAnsiReset . withStdColours $ do
    setSGR [SetUnderlining DoubleUnderline, SetColor Foreground Vivid Cyan]
    title svcName
    newline
    title underline
    newline
    stdColours
    propertyNameContent "Name" $ cs ^. ECS.csServiceName
    propertyNameContent "ARN" $ cs ^. ECS.csServiceARN
    propertyNameContent "Role ARN" $ cs ^. ECS.csRoleARN
    propertyNameContent "TaskDef" $ cs ^. ECS.csTaskDefinition
    propertyNameContent "Running" $ show <$> cs ^. ECS.csRunningCount
    propertyNameContent "Desired" $ show <$> cs ^. ECS.csDesiredCount
    propertyNameContent "Created" $ show <$> cs ^. ECS.csCreatedAt
    propertyName "Load balancers" >> newline
    mapM_ (disp @ 'Terminal) $ cs ^. ECS.csLoadBalancers
   where
    svcName   = fromMaybe "--" $ cs ^. ECS.csServiceName
    underline = T.replicate (T.length svcName) "_"

instance Disp 'Terminal ECS.LoadBalancer where
  disp lb = withAnsiReset . withStdColours $ do
    propertyName . fromMaybe "(No name)" $ lb ^. ECS.lbLoadBalancerName
    newline
    propertyNameContent "Target group ARN" $ lb ^. ECS.lbTargetGroupARN
    propertyNameContent "Container name" $ lb ^. ECS.lbContainerName
    propertyNameContent "Container port" $ show <$> lb ^. ECS.lbContainerPort



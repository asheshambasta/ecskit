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

import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T

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

instance (Typeable a, Disp 'Terminal a) => Disp 'Terminal (Maybe a) where
  disp Nothing = withAnsiReset . withStdColours $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn @Text msg
   where
    msg      = typeName <> " not found."
    typeName = show . typeRep $ Proxy @a
  disp (Just a) = disp @ 'Terminal a

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
    heading svcName
    newline
    stdColours
    propertyNameContent "Name" $ cs ^. ECS.csServiceName
    propertyNameContent "ARN" $ cs ^. ECS.csServiceARN
    propertyNameContent "Role ARN" $ cs ^. ECS.csRoleARN
    propertyNameContent "TaskDef" $ cs ^. ECS.csTaskDefinition
    propertyNameContent "Launch type" $ show <$> cs ^. ECS.csLaunchType
    propertyNameContent "Running" $ show <$> cs ^. ECS.csRunningCount
    propertyNameContent "Desired" $ show <$> cs ^. ECS.csDesiredCount
    propertyNameContent "Created" $ show <$> cs ^. ECS.csCreatedAt
    propertyName "Load balancers" >> newline
    mapM_ (disp @ 'Terminal) $ cs ^. ECS.csLoadBalancers
    where svcName = fromMaybe "--" $ cs ^. ECS.csServiceName

instance Disp 'Terminal ECS.LoadBalancer where
  disp lb = withAnsiReset . withStdColours $ do
    propertyName . fromMaybe "(No name)" $ lb ^. ECS.lbLoadBalancerName
    newline
    propertyNameContent "Target group ARN" $ lb ^. ECS.lbTargetGroupARN
    propertyNameContent "Container name" $ lb ^. ECS.lbContainerName
    propertyNameContent "Container port" $ show <$> lb ^. ECS.lbContainerPort

instance Disp 'Terminal ECS.TaskDefinition where
  disp td = withAnsiReset . withStdColours $ do
    propertyNameContent "Status" $ T.drop 3 . show <$> td ^. ECS.tdStatus
    propertyNameContent "CPU" $ td ^. ECS.tdCpu
    propertyNameContent "Memory" $ td ^. ECS.tdMemory
    propertyNameContent "Family" $ td ^. ECS.tdFamily
    propertyNameContent "Network mode" $ show <$> td ^. ECS.tdNetworkMode
    propertyNameContent "Revision" $ show <$> td ^. ECS.tdRevision
    propertyNameContent "Execution role ARN" $ td ^. ECS.tdExecutionRoleARN
    propertyNameContent "Task role ARN" $ td ^. ECS.tdTaskRoleARN

    propertyNameContent "Requires compats" rCompats
    propertyNameContent "Compats"          compats
    setSGR [SetColor Foreground Vivid Yellow]
    title "Container definitions"
    newline
    mapM_ (disp @ 'Terminal) (td ^. ECS.tdContainerDefinitions)
   where
    compats =
      Just
        .  T.intercalate ", "
        .  fmap (T.drop 1 . show)
        $  td
        ^. ECS.tdCompatibilities
    rCompats =
      Just
        .  T.intercalate ", "
        .  fmap (T.drop 1 . show)
        $  td
        ^. ECS.tdRequiresCompatibilities

instance Disp 'Terminal ECS.ContainerDefinition where
  disp cd = withAnsiReset $ do
    setSGR [SetColor Foreground Vivid Yellow]
    propertyNameContent "Name" $ cd ^. ECS.cdName
    propertyNameContent "Image" $ cd ^. ECS.cdImage
    propertyNameContent "Privileged" $ show <$> cd ^. ECS.cdPrivileged
    propertyNameContent "Essential" $ show <$> cd ^. ECS.cdEssential
    propertyNameContent "Memory (MiB)" $ show <$> cd ^. ECS.cdMemory
    propertyNameContent "CPU" $ show <$> cd ^. ECS.cdCpu
    dnsServers
    portMappings
    environment
    title "Command"
    putStrLn $ indented addSlash (cd ^. ECS.cdCommand)
    entryPoint
    logConf
   where

    addSlash t = t <> " \\"
    portMappings = unless (null $ cd ^. ECS.cdPortMappings) $ do
      propertyName "Port mappings"
      newline
      let portMapping pm = do
            propertyNameContent "Proto" $ show <$> pm ^. ECS.pmProtocol
            putStrLn . indentedNoLeadingNewline identity $ T.unwords
              [ "(Host)"
              , showPort $ pm ^. ECS.pmHostPort
              , "→ "
              , showPort $ pm ^. ECS.pmContainerPort
              , "(Container)"
              ]
          showPort = maybe "—" show
      mapM_ portMapping $ cd ^. ECS.cdPortMappings
    environment = unless (null $ cd ^. ECS.cdEnvironment) $ do
      propertyName "Environment"
      newline
      let showEnv kv = fromMaybe "—" (kv ^. ECS.kvpName) <> "=" <> fromMaybe
            "—"
            (kv ^. ECS.kvpValue)
      putStrLn . indented identity $ showEnv <$> cd ^. ECS.cdEnvironment

    dnsServers =
      unless (null $ cd ^. ECS.cdDnsServers)
        .  propertyNameContent "DNS Servers"
        .  Just
        .  T.intercalate ", "
        $  cd
        ^. ECS.cdDnsServers

    entryPoint =
      let ep = cd ^. ECS.cdEntryPoint
      in  unless (null ep) $ do
            title "Entrypoint"
            putStrLn $ indented addSlash (cd ^. ECS.cdEntryPoint)

    logConf = case cd ^. ECS.cdLogConfiguration of
      Just lc -> do
        propertyNameContent "Log driver" . Just . show $ lc ^. ECS.lcLogDriver
        putStrLn . indented identity $ logOptions
       where
        logOptions =
          [ k <> ": " <> v | (k, v) <- HM.toList (lc ^. ECS.lcOptions) ]
      Nothing -> pure ()

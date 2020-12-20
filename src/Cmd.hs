{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
  , StandaloneDeriving
  , TupleSections
#-}
module Cmd
  ( Cmd(..)
  , AnyCmd(..)
  , UpdateMode(..)
  , describeServicesCmd
  , describeClustersCmd
  , listAllServicesCmd
  , listTaskDefsCmd
  , updateTaskDefsCmd
  , describeUsedTaskDefsCmd
  , runCmd
  , runCmdExplicit
  , module AWS.Types
  ) where

import           Data.Default.Class             ( Default(..) )
import qualified Data.Map                      as M

import           Lib.List                       ( groupedIn )
import           Prelude                 hiding ( to )

import           Cmd.Results

import           Polysemy
import           Polysemy.AWS
import           Polysemy.Reader
import           Polysemy.Writer

import           Control.Lens
import           Control.Monad                  ( (<=<) )
import qualified Data.List                     as L
import qualified Data.Text                     as T

import "this"    AWS.Commands.ECR              as ECR
import "this"    AWS.Commands.TaskDef
import "this"    AWS.Types

import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS.DescribeClusters
                                               as ECS
import qualified Network.AWS.ECS.DescribeServices
                                               as ECS
import qualified Network.AWS.ECS.ListServices  as ECS
import qualified Network.AWS.ECS.Types         as ECS
import qualified Network.AWS.ECS.UpdateService as ECS

data AnyCmd where
  AnyCmd ::Cmd m a -> AnyCmd

deriving instance Show AnyCmd

data Cmd m a where
  DescribeServicesCmd ::ClusterName -> NonEmpty ServiceName -> Cmd m [ServiceDescription]
  DescribeClustersCmd ::ECS.DescribeClusters -> Cmd m ECS.DescribeClustersResponse
  ListAllServicesCmd ::ClusterName -> Maybe ECS.LaunchType -> Cmd m [Arn 'AwsService]
  ListTaskDefsCmd ::TaskDefFamily -> Maybe ECS.TaskDefinitionStatus -> Cmd m [Arn 'AwsTaskDef]
  -- | Update the task definitions to the latest task def, or to a revision specified.
  -- 1. If no services are specified, all services of the given cluster are updated to the latest revisions of the TaskDefs.
  -- 2. Non-homogenous per-service task defs. are not supported, in these cases, we leave the service unchanged.
  -- 3. The `UpdateMode` is used to specify the AWS equivalent of @--force-new-update@
  UpdateTaskDefsCmd ::ClusterName
                    -> [(ServiceName, Maybe Int)]
                    -> Maybe UpdateMode
                    -> Cmd m [UpdateTaskDefResult]
  -- | Describe the used task definition by one or more services.
  DescribeUsedTaskDefsCmd ::ClusterName -> NonEmpty ServiceName -> Cmd m [(ServiceName, Maybe ECS.TaskDefinition)]

  -- | Goes through a service's container definitions to find ECR images, and describes these images
  DescribeServiceImagesCmd ::ClusterName -> NonEmpty ServiceName -> Cmd m (Map ServiceName ServiceContainerImages)

makeSem ''Cmd
deriving instance Show (Cmd m a)

-- | Interpret an AWS Cmd.
runCmd
  :: forall a r
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Sem (Cmd ': r) a
  -> Sem r a
runCmd = interpret $ runCmdExplicit >=> pure . \case
  DescribeServicesResult     svcs -> svcs
  DescribeClustersResult     cs   -> cs
  ListServicesResult         svcs -> svcs
  ListTaskDefsResult         tds  -> tds
  UpdateTaskDefsResult       tdrs -> tdrs
  DescribeUsedTaskDefsResult r    -> r

runCmdExplicit
  :: forall a r m
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => Cmd m a
  -> Sem r (CmdResult a)
runCmdExplicit cmd = case cmd of
  DescribeServicesCmd cluster services -> do
    containers <- foldM allContainers mempty svcsGrouped
    let mkDesc container =
          let mtd = taskDefArn <$> container ^. ECS.csTaskDefinition
          in  ServiceDescription container
                <$> maybe (pure Nothing)
                          (fmap Just . runTaskDefCmd . tdGetServiceTaskDef)
                          mtd
    DescribeServicesResult <$> mapM mkDesc containers
   where
    -- AWS limits to a max. 10 services per describe-service query.
    svcsGrouped = groupedIn 10 (toList services)
    allContainers acc svcGroup =
      let ds =
            ECS.describeServices
              & (ECS.dCluster ?~ unName cluster)
              & (ECS.dServices .~ toList (unName <$> svcGroup))
      in  mappend acc . view ECS.dssrsServices <$> liftAWS (AWS.send ds)

  DescribeClustersCmd dcs -> DescribeClustersResult <$> liftAWS (AWS.send dcs)
  ListAllServicesCmd cname lt ->
    ListServicesResult
      .   concatMap (fmap ServiceArn . view ECS.lsrsServiceARNs)
      <$> collectAWSResponses initReq
                              (\ls t -> ls & ECS.lsNextToken ?~ t)
                              (view ECS.lsrsNextToken)
   where
    initReq =
      ECS.listServices
        & (ECS.lsCluster ?~ unName cname)
        & (ECS.lsLaunchType .~ lt)

  ListTaskDefsCmd f mStatus ->
    ListTaskDefsResult <$> runTaskDefCmd (tdListTaskDefs f mStatus)

  UpdateTaskDefsCmd cluster svcRevs (fromMaybe def -> uMode) ->
    runCmdExplicit (ListAllServicesCmd cluster Nothing) >>= \case
      ListServicesResult serviceArns ->
        fmap (UpdateTaskDefsResult . fst)
          . runWriter @[UpdateTaskDefResult]
          $ updateTaskDefsWith cluster svcRevs uMode serviceArns

  DescribeUsedTaskDefsCmd c svcs ->
    runCmdExplicit (DescribeServicesCmd c svcs) >>= \case
      DescribeServicesResult descs ->
        DescribeUsedTaskDefsResult <$> taskDefsFromDescs descs

  DescribeServiceImagesCmd c svcs ->
    -- first get the full service desc. 
    runCmdExplicit (DescribeServicesCmd c svcs) >>= \case
      DescribeServicesResult descs -> taskDefsFromDescs descs >>= taskDefImages

updateTaskDefsWith
  :: forall r
   . ( Members
         '[ Reader AWS.Env
          , Embed IO
          , Error AWSError
          , Writer [UpdateTaskDefResult]
          ]
         r
     )
  => ClusterName
  -> [(ServiceName, Maybe Int)]
  -> UpdateMode
  -> [Arn 'AwsService]
  -> Sem r ()
updateTaskDefsWith cluster svcRevs uMode serviceArns = do
      -- the user supplied service names may or may not be the full service ARNs.
  let
    invalidSvcs =
      (`ServiceFailed` "Service not found.")
        <$> filter (not . isValid) givenSvcs
    serviceTexts = arnText <$> serviceArns
    isValid (unName -> svc) = isJust . find (svc `T.isSuffixOf`) $ serviceTexts
    -- these are services we want to update: if the user didn't specify any svcs, we update all
    -- in the cluster.
    toUpdate = if null svcRevs
      then (, Nothing) . ServiceName <$> serviceTexts
      else filter (isValid . fst) svcRevs

  -- report the invalid services
  tell @[UpdateTaskDefResult] invalidSvcs
  maybe (pure ()) updateServiceRevs (nonEmpty toUpdate)

 where
  givenSvcs = fst <$> svcRevs
  updateServiceRevs updSvcRevs =
    let svcNames = fst <$> updSvcRevs
    in  runCmdExplicit (DescribeServicesCmd cluster svcNames) >>= \case
          DescribeServicesResult descs -> mapM_ updateService descs
  updateService :: ServiceDescription -> Sem r ()
  updateService (ServiceDescription cs mtd) = case mtd of
    Nothing -> tell' noTaskDefs
    -- only process the given svc. if homogenous.
    Just ServiceTaskDef {..} | homogenousTaskDefs _sdAvailTaskDefs ->
      -- find if the user wanted to roll the service to a particular revision.
      let mRev = join $ L.lookup svcName svcRevs
      in  maybe updateToLatest updateToRevision mRev
     where
      updateToLatest = case latestTaskDef _sdAvailTaskDefs of
        Nothing -> tell' noTaskDefs
        -- if the task-def in use is the latest, only update on `Force`. 
        Just td | td /= _sdCurTaskDef || uMode == Force ->
          infoMsg td >> callAWS td
        Just _ -> tell' nothingToDo
      updateToRevision rev =
        let tdArnOld              = TaskDefArn (Just rev) tdFamily
            TaskDefArn _ tdFamily = _sdCurTaskDef
        in  infoMsg tdArnOld >> callAWS tdArnOld
      callAWS td'
        | uMode == DryRun = pure ()
        | otherwise = do
          res <- runTaskDefCmd $ tdUpdateTaskDef cluster svcName uMode td'
          -- ECS may respond with no `ECS.ContainerInstance` value, in this case, we must
          -- attempt to describe the service again. If we're not that unlucky, we want to
          -- use the returned `ECS.ContainerService` value to report the new service. 
          maybe redescribe (fromContainerService td') $ res ^. ECS.usrsService

      redescribe =
        warnRedesc
          >>  runCmdExplicit (DescribeServicesCmd cluster (pure svcName))
          >>= \case
                DescribeServicesResult descs ->
                  tell @[UpdateTaskDefResult] (ServiceSuccess svcName <$> descs)

      fromContainerService td' cs' =
        -- fetch the task-def data again, and embed that in a new ServiceDescription.
        serviceTaskDef td'
          >>= tell'
          .   ServiceSuccess svcName
          .   ServiceDescription cs'
          .   Just

      warnRedesc =
        tell'
          . ServiceWarn svcName
          $ "No ContainerService returned from ECS on `update-service`."

      infoMsg td' =
        tell'
          .  ServiceInfo svcName
          $  "["
          <> show uMode
          <> "] Will update: "
          <> arnText _sdCurTaskDef
          <> " →  "
          <> arnText td'

    Just std -> tell' $ nonHomogenous std
   where
    svcName = ServiceName . fromMaybe "UNKNOWN" $ cs ^. ECS.csServiceName
    noTaskDefs =
      ServiceFailed svcName "Cannot get ServiceTaskDef from DescribeService."
    nonHomogenous std =
      ServiceFailed svcName $ "Non-Homogenous taskdefs: " <> show std
    nothingToDo = ServiceFailed
      svcName
      "Already using the latest TaskDef, use `Force` to override."

  tell' = tell @[UpdateTaskDefResult] . pure

taskDefImages
  :: forall r
   . (Members '[Reader AWS.Env , Embed IO , Error AWSError] r)
  => [(ServiceName, Maybe ECS.TaskDefinition)]
  -> Sem r (CmdResult (Map ServiceName ServiceContainerImages))
taskDefImages = fmap (DescribeServiceImagesResult . M.fromList)
  . mapM maybeImages
 where
  maybeImages (sName, Nothing) = pure (sName, ServiceContainerImages mempty)
  maybeImages (sName, Just td) =
    fmap ((sName, ) . ServiceContainerImages)
      . runEcrCmd
      . ecrDescribeTaskDefImages
      $ td


{-# LANGUAGE
    TemplateHaskell
  , TypeApplications
  , TypeOperators
  , PolyKinds
  , TupleSections
  , StandaloneDeriving
#-}
module AWS.Commands.TaskDef
  ( tdListTaskDefs
  , tdGetServiceTaskDef
  , tdDescribeTaskDefs
  , tdUpdateTaskDef
  , serviceTaskDef
  , runTaskDefCmd
  , TaskDefCmd(..)
  , UpdateMode(..)
  ) where

import           Control.Lens
import           Data.Default.Class             ( Default(..) )

import "this"    AWS.Types
import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS               as ECS

import           Polysemy
import           Polysemy.AWS
import           Polysemy.Reader

data UpdateMode =
  Force     -- ^ --force-new-deployment 
  | NoForce -- ^ No --force-new-deployment
  | DryRun  -- ^ Just output the upgrade information and exit.
  deriving (Eq, Show, Read, Enum, Bounded)

instance Default UpdateMode where
  def = DryRun

data TaskDefCmd m a where
  TdListTaskDefs ::TaskDefFamily -> Maybe ECS.TaskDefinitionStatus -> TaskDefCmd m [Arn 'AwsTaskDef]
  TdGetServiceTaskDef ::Arn 'AwsTaskDef -> TaskDefCmd m ServiceTaskDef
  TdDescribeTaskDefs ::[TaskDefName] -> TaskDefCmd m [(TaskDefName, Maybe ECS.TaskDefinition)]
  TdUpdateTaskDef ::ClusterName -> ServiceName -> UpdateMode -> Arn 'AwsTaskDef -> TaskDefCmd m ECS.UpdateServiceResponse

deriving instance Show (TaskDefCmd m a)

makeSem ''TaskDefCmd

runTaskDefCmd
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => Sem (TaskDefCmd ': r) a
  -> Sem r a
runTaskDefCmd = interpret $ \case
  TdListTaskDefs tdf mStatus      -> listTaskDefs' tdf mStatus
  TdGetServiceTaskDef tdArn       -> serviceTaskDef tdArn
  TdDescribeTaskDefs  names       -> foldM tdDescribe' mempty names
  TdUpdateTaskDef c s uMode tdArn -> liftAWS . AWS.send $ updateTaskDef
   where
    updateTaskDef =
      ECS.updateService (unName s)
        & (ECS.usCluster ?~ unName c)
        & (ECS.usTaskDefinition ?~ arnText tdArn)
        & (ECS.usForceNewDeployment ?~ (uMode == Force))

tdDescribe'
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => [(TaskDefName, Maybe ECS.TaskDefinition)]
  -> TaskDefName
  -> Sem r [(TaskDefName, Maybe ECS.TaskDefinition)]
tdDescribe' acc name@(Name td) =
  let d = ECS.describeTaskDefinition td
  in  mappend acc . pure . (name, ) . view ECS.desrsTaskDefinition <$> liftAWS
        (AWS.send d)

-- | List all task defs based on a TaskDef family.
listTaskDefs'
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => TaskDefFamily
  -> Maybe ECS.TaskDefinitionStatus
  -> Sem r [Arn 'AwsTaskDef]
listTaskDefs' (TaskDefFamily f) mStatus =
  let req =
        ECS.listTaskDefinitions
          & (ECS.ltdStatus .~ mStatus)
          & (ECS.ltdFamilyPrefix ?~ f)
  in  mkArns <$> collectAWSResponses req
                                     (\ltds t -> ltds & ECS.ltdNextToken ?~ t)
                                     (view ECS.ltdrsNextToken)
  where mkArns = concatMap (fmap taskDefArn . view ECS.ltdrsTaskDefinitionARNs)

-- | List all task definitions available for a service.
serviceTaskDef
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => Arn 'AwsTaskDef -- ^ The current task def. in use by the service
  -> Sem r ServiceTaskDef
serviceTaskDef arn@(TaskDefArn _ (TaskDefFamilyArn f _)) =
  ServiceTaskDef arn . sortOn Down <$> listTaskDefs' f Nothing


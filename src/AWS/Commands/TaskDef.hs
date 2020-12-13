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
  , serviceTaskDef
  , runTaskDefCmd
  , TaskDefCmd(..)
  ) where

import           Control.Lens

import "this"    AWS.Types
import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS               as ECS

import           Polysemy
import           Polysemy.AWS
import           Polysemy.Reader

data TaskDefCmd m a where
  TdListTaskDefs ::TaskDefFamily -> Maybe ECS.TaskDefinitionStatus -> TaskDefCmd m [Arn 'AwsTaskDef]
  TdGetServiceTaskDef ::Arn 'AwsTaskDef -> TaskDefCmd m ServiceTaskDef
  TdDescribeTaskDefs ::[TaskDefName] -> TaskDefCmd m [(TaskDefName, Maybe ECS.TaskDefinition)]

deriving instance Show (TaskDefCmd m a)

makeSem ''TaskDefCmd

runTaskDefCmd
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => Sem (TaskDefCmd ': r) a
  -> Sem r a
runTaskDefCmd = interpret $ \case
  TdListTaskDefs tdf mStatus -> listTaskDefs' tdf mStatus
  TdGetServiceTaskDef tdArn  -> serviceTaskDef tdArn
  TdDescribeTaskDefs  names  -> foldM tdDescribe' mempty names

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


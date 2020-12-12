{-# LANGUAGE
    TemplateHaskell
  , BlockArguments
  , StrictData
  , TypeApplications
  , TypeOperators
  , DataKinds
  , PolyKinds
  , StandaloneDeriving
  , PatternSynonyms
#-}
module AWS.Types
  ( ClusterName(..)
  , TaskDefFamily(..)
  , pattern TaskDefFamily
  -- * ARNs
  , ArnType(..)
  , Arn(..)
  , ServiceTaskDef(..)
  -- ** Displaying ARNs
  , arnText
  -- * Parsing specific ARNs
  , taskDefArn
  -- * Listing task defs.
  , listTaskDefs
  , serviceTaskDef
  ) where

import           Control.Lens

import qualified Network.AWS                   as AWS
import qualified Network.AWS.ECS               as ECS

import           Polysemy.Reader
import           Polysemy
import           Polysemy.AWS

import           Cmd.Disp.ANSI.Helpers
import           Cmd.Disp

import qualified Data.Text                     as T

data ArnType = ArnService
             | ArnTaskDef
             deriving (Show, Eq)

data Arn (t :: ArnType) where
  -- | A simple service ARN
  ServiceArn ::Text -> Arn 'ArnService
  -- | Task definition ARN: contains the task def ARN part and the revision, if supplied.
  TaskDefArn ::Maybe Int -> TaskDefFamily -> Text -> Arn 'ArnTaskDef

deriving instance Eq (Arn t)
deriving instance Show (Arn t)
deriving instance Ord (Arn t)

arnText :: Arn t -> Text
arnText = \case
  ServiceArn t -> t
  TaskDefArn rev (TaskDefFamily f) rest ->
    rest <> f <> ":" <> maybe "" show rev

-- | Splits the Text at the last ":" and tries to parse the RHS as Int.
taskDefArn :: Text -> Arn 'ArnTaskDef
taskDefArn txt =
  let (arnFamily, rev                    ) = T.breakOnEnd ":" txt
      (rest     , TaskDefFamily -> family) = T.breakOnEnd "/" arnFamily
  in  if T.null rev
        then TaskDefArn Nothing family rest
        else TaskDefArn (readMaybe $ T.unpack rev) family rest

-- | Status of the service's TaskDefintion.
data ServiceTaskDef = ServiceTaskDef
  { _sdCurTaskDef    :: Arn 'ArnTaskDef
  , _sdAvailTaskDefs :: [Arn 'ArnTaskDef]
  }
  deriving (Eq, Show)

instance Disp 'Terminal ServiceTaskDef where
  disp std@ServiceTaskDef {..} = withAnsiReset . withStdColours $ do
    propertyName "TaskDefs"
    newline
    propertyNameContent "In-use"      (Just $ arnText _sdCurTaskDef)
    propertyNameContent "UsingLatest" (show <$> latestTaskDef std)
    putStrLn allDefs
    where allDefs = indented arnText _sdAvailTaskDefs

latestTaskDef :: ServiceTaskDef -> Maybe Bool
latestTaskDef ServiceTaskDef {..} =
  (_sdCurTaskDef ==) <$> headMay (sortOn Down _sdAvailTaskDefs)

-- | List all task definitions available for a service.
serviceTaskDef
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => Arn 'ArnTaskDef -- ^ The current task def. in use by the service
  -> Sem r ServiceTaskDef
serviceTaskDef arn@(TaskDefArn _ f _) =
  ServiceTaskDef arn . sortOn Down <$> listTaskDefs f Nothing

-- | List all task defs based on a TaskDef family.
listTaskDefs
  :: Members '[Reader AWS.Env , Embed IO , Error AWSError] r
  => TaskDefFamily
  -> Maybe ECS.TaskDefinitionStatus
  -> Sem r [Arn 'ArnTaskDef]
listTaskDefs (TaskDefFamily f) mStatus =
  let req =
        ECS.listTaskDefinitions
          & (ECS.ltdStatus .~ mStatus)
          & (ECS.ltdFamilyPrefix ?~ f)
  in  mkArns <$> collectAWSResponses req
                                     (\ltds t -> ltds & ECS.ltdNextToken ?~ t)
                                     (view ECS.ltdrsNextToken)
  where mkArns = concatMap (fmap taskDefArn . view ECS.ltdrsTaskDefinitionARNs)

-- | Cluster name
newtype ClusterName = ClusterName { unClusterName :: Text}
                    deriving (Eq, Show, IsString) via Text

newtype TaskDefFamily = UnsafeTaskDefFamily { unTaskDefFamily :: Text }
                      deriving (Eq, Show, IsString, Ord) via Text

{-# COMPLETE TaskDefFamily #-}
pattern TaskDefFamily :: Text -> TaskDefFamily
pattern TaskDefFamily f <- UnsafeTaskDefFamily f where
  TaskDefFamily f =
    UnsafeTaskDefFamily . T.dropWhile (== ':') . T.dropWhileEnd (== ':') $ f

instance Disp 'Terminal [Arn t] where
  disp arns =
    let arnsStr = indented arnText arns
    in  withAnsiReset . withStdColours $ propertyNameContent "ARNs"
                                                             (Just arnsStr)


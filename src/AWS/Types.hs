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
  ( -- * Names
    Name(..)
  , ClusterName
  , ServiceName
  , TaskDefName
  , TaskDefFamily(..)
  , pattern TaskDefFamily
  -- * ARNs
  , AwsType(..)
  , Arn(..)
  , ServiceTaskDef(..)
  -- ** Displaying ARNs
  , arnText
  -- * Parsing specific ARNs
  , taskDefArn
  -- * Listing task defs.
  , homogenousTaskDefs
  , latestTaskDef
  , isLatestTaskDef
  ) where

import           Cmd.Disp
import           Cmd.Disp.ANSI.Helpers

import qualified Data.Text                     as T

data AwsType = AwsService
             | AwsCluster
             | AwsTaskDef
             | AwsTaskDefFamily
             deriving (Show, Eq)

data Arn (t :: AwsType) where
  -- | A simple service ARN
  ServiceArn ::Text -> Arn 'AwsService
  -- | Task definition ARN: contains the task def ARN part and the revision, if supplied.
  TaskDefArn ::Maybe Int -> Arn 'AwsTaskDefFamily -> Arn 'AwsTaskDef
  TaskDefFamilyArn ::TaskDefFamily -> Text -> Arn 'AwsTaskDefFamily

deriving instance Eq (Arn t)
deriving instance Show (Arn t)
deriving instance Ord (Arn t)

arnText :: Arn t -> Text
arnText = \case
  ServiceArn t -> t
  TaskDefFamilyArn (TaskDefFamily f) rest -> rest <> f
  TaskDefArn rev f -> arnText f <> ":" <> maybe "" show rev

-- | Splits the Text at the last ":" and tries to parse the RHS as Int.
taskDefArn :: Text -> Arn 'AwsTaskDef
taskDefArn txt =
  let (arnFamily, rev                    ) = T.breakOnEnd ":" txt
      (rest     , TaskDefFamily -> family) = T.breakOnEnd "/" arnFamily
  in  if T.null rev
        then TaskDefArn Nothing $ TaskDefFamilyArn family rest
        else TaskDefArn (readMaybe $ T.unpack rev)
          $ TaskDefFamilyArn family rest

newtype Name (t :: AwsType) = Name { unName :: Text } deriving (Eq, Show, Ord, IsString) via Text
type ClusterName = Name 'AwsCluster
type ServiceName = Name 'AwsService
type TaskDefName = Name 'AwsTaskDef

-- | Status of the service's TaskDefintion.
data ServiceTaskDef = ServiceTaskDef
  { _sdCurTaskDef    :: Arn 'AwsTaskDef
  , _sdAvailTaskDefs :: [Arn 'AwsTaskDef]
  }
  deriving (Eq, Show)

instance Disp 'Terminal ServiceTaskDef where
  disp std@ServiceTaskDef {..} = withAnsiReset . withStdColours $ do
    propertyName "TaskDefs"
    newline
    propertyNameContent "In-use"      (Just $ arnText _sdCurTaskDef)
    propertyNameContent "UsingLatest" (show <$> isLatestTaskDef std)
    putStrLn allDefs
    where allDefs = indented arnText _sdAvailTaskDefs

latestTaskDef :: [Arn 'AwsTaskDef] -> Maybe (Arn 'AwsTaskDef)
latestTaskDef tds | null tds  = Nothing
                  | otherwise = Just $ maximum tds

isLatestTaskDef :: ServiceTaskDef -> Maybe Bool
isLatestTaskDef ServiceTaskDef {..} =
  (_sdCurTaskDef ==) <$> headMay (sortOn Down _sdAvailTaskDefs)

-- | Check if a given list of task definitions is homogenous (contains revisions of the same family)
homogenousTaskDefs :: [Arn 'AwsTaskDef] -> Bool
homogenousTaskDefs arns =
  let families = [ f | TaskDefArn _ f <- arns ]
  in  case headMay families of
        Nothing -> True
        Just h  -> all (== h) families

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

instance Disp 'Terminal (Name n) where
  disp (Name n) = setSGR [SetColor Foreground Vivid Cyan] >> heading n

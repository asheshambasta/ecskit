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
  , ContainerDefName
  , EcrRepoName
  , pattern ClusterName
  , pattern ServiceName
  , pattern TaskDefName
  , pattern ContainerDefName
  , pattern EcrRepoName
  , parseEcrRepo
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

import qualified Data.Char                     as C
import qualified Lib.Parse                     as P
import qualified Text.Megaparsec               as P
import           Cmd.Disp
import           Cmd.Disp.ANSI.Helpers

import qualified Data.Text                     as T

data AwsType = AwsService
             | AwsCluster
             | AwsTaskDef
             | AwsEcsContainerDef
             | AwsTaskDefFamily
             | AwsEcrRepo
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

{-# COMPLETE ClusterName, ServiceName, TaskDefName, EcrRepoName, ContainerDefName #-}
newtype Name (t :: AwsType) name = UnsafeName { unName :: name } deriving (Eq, Show, Ord, IsString) via name

type ClusterName = Name 'AwsCluster Text
type ServiceName = Name 'AwsService Text
type TaskDefName = Name 'AwsTaskDef Text
type ContainerDefName = Name 'AwsEcsContainerDef Text

{- | ECR repos are structured as:
@@
<_ecrrRegId>.dkr.ecr.<region>.amazonaws.com/<_ecrrRepo>:<_ecrrTag>
            |--          _ecrrHost       --|
@@

RegId values are numeric.
-}
data EcrRepo = EcrRepo
  { _ecrrRegId :: Text
  , _ecrrHost  :: Text
  , _ecrrRepo  :: Text
  , _ecrrTag   :: Text
  }
  deriving (Eq, Show)

parseEcrRepo :: P.ParserText EcrRepo
parseEcrRepo = do
  _ecrrRegId <- P.takeWhile1P (Just "_ecrrRegId") C.isNumber
  P.char '.'
  _ecrrHost <- P.takeWhile1P (Just "_ecrrHost") (/= '/')
  P.char '/'
  _ecrrRepo <- P.takeWhile1P (Just "_ecrrRepo") (/= ':')
  P.char ':'
  _ecrrTag <- P.takeRest
  pure EcrRepo { .. }
-- ecrRepo :: Text ->  EcrRepo
-- ecrRepo reg =
--   let (pre, _ecrrTag) = T.breakOnEnd ":" reg
--       -- the registry id is a number
--       _ecrrRegId      = T.takeWhile C.isNumber pre
--       _ecrrRepo       = T.takeWhileEnd (/= '/') pre
--       _ecrrHost       = T.dropk
--   in  undefined

-- pattern EcrRepo' :: Text -> Maybe EcrRepo
-- pattern EcrRepo' n 

type EcrRepoName = Name 'AwsEcrRepo Text

pattern ClusterName :: Text -> ClusterName
pattern ClusterName n <- UnsafeName n where
  ClusterName n = UnsafeName n

pattern ServiceName :: Text -> ServiceName
pattern ServiceName n <- UnsafeName n where
  ServiceName n = UnsafeName n

pattern TaskDefName :: Text -> TaskDefName
pattern TaskDefName n <- UnsafeName n where
  TaskDefName n = UnsafeName n

pattern ContainerDefName :: Text -> ContainerDefName
pattern ContainerDefName n <- UnsafeName n where
  ContainerDefName n = UnsafeName n

pattern EcrRepoName :: Text -> EcrRepoName
pattern EcrRepoName n <- UnsafeName n where
  EcrRepoName n =
    let (pre, _) = T.breakOnEnd ":" n
        takeName = UnsafeName . T.takeWhileEnd (/= '/')
    in  takeName $ if T.null pre then n else T.dropEnd 1 pre

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

instance Disp 'Terminal (Name t Text) where
  disp (UnsafeName n) = setSGR [SetColor Foreground Vivid Cyan] >> heading n

instance {-# OVERLAPPING #-} Disp 'Terminal ContainerDefName where
  disp (UnsafeName (mappend "ContainerDef: " -> n)) =
    setSGR [SetColor Foreground Vivid Yellow] >> heading n

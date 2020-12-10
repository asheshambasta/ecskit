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

import           System.Console.ANSI
import           Control.Lens
import qualified Network.AWS.ECS.Types         as ECS

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

-- | Safely reset the terminal after some IO.
withAnsiReset :: IO a -> IO a
withAnsiReset = try @SomeException >=> either resetThrow resetPure
 where
  resetPure a = setSGR [Reset] $> a
  resetThrow err = setSGR [Reset] >> throwIO err

instance Disp 'Terminal ECS.Cluster where
  disp c = withAnsiReset $ do
    setSGR [SetColor Foreground Vivid White, SetColor Background Vivid Black]
    withTitle "Name"   (c ^. ECS.cClusterName)
    withTitle "ARN"    (c ^. ECS.cClusterARN)
    withTitle "Status" (c ^. ECS.cStatus)
    withTitle "Active services"
              (show @Int @Text <$> c ^. ECS.cActiveServicesCount)
    withTitle "Running tasks" $ show @Int @Text <$> c ^. ECS.cRunningTasksCount
   where
    withTitle title mContent = do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStr @Text $ title <> ": "
      setSGR [SetConsoleIntensity NormalIntensity]
      putStrLn . fromMaybe "--" $ mContent


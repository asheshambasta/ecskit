module Cmd.Disp.ANSI.Helpers
  ( withAnsiReset
  , newline
  , propertyName
  , propertyNameContent
  , withStdColours
  , module System.Console.ANSI
  ) where

import           System.Console.ANSI

-- | Safely reset the terminal after some IO.
withAnsiReset :: IO a -> IO a
withAnsiReset = try @SomeException >=> either resetThrow resetPure
 where
  resetPure a = setSGR [Reset] $> a
  resetThrow err = setSGR [Reset] >> throwIO err

newline :: IO ()
newline = putStrLn @Text ""

propertyName :: Text -> IO ()
propertyName title = do
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr $ title <> ": "
  setSGR [SetConsoleIntensity NormalIntensity]

propertyNameContent :: Text -> Maybe Text -> IO ()
propertyNameContent title mContent = do
  propertyName title
  putStrLn . fromMaybe "--" $ mContent

withStdColours :: IO a -> IO a
withStdColours op =
  setSGR [SetColor Foreground Vivid White, SetColor Background Vivid Black]
    >> op

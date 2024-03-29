module Cmd.Disp.ANSI.Helpers
  ( withAnsiReset
  , heading
  , title
  , newline
  , propertyName
  , propertyNameContent
  , withStdColours
  , stdColours
  , indented
  , indentedNoLeadingNewline
  , module System.Console.ANSI
  ) where

import qualified Data.Text                     as T
import           System.Console.ANSI

-- | Safely reset the terminal after some IO.
withAnsiReset :: IO a -> IO a
withAnsiReset = try @SomeException >=> either resetThrow resetPure
 where
  resetPure a = setSGR [Reset] $> a
  resetThrow err = setSGR [Reset] >> throwIO err

newline :: IO ()
newline = putStrLn @Text ""

heading :: Text -> IO ()
heading h = title h >> newline >> putStrLn (T.replicate (T.length h) "_")

title :: Text -> IO ()
title t = do
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr t
  setSGR [SetConsoleIntensity NormalIntensity]

propertyName :: Text -> IO ()
propertyName t = do
  title $ t <> ": "
  setSGR [SetConsoleIntensity NormalIntensity]

propertyNameContent :: Text -> Maybe Text -> IO ()
propertyNameContent t mContent = do
  propertyName t
  putStrLn . fromMaybe "--" $ mContent

withStdColours :: IO a -> IO a
withStdColours op = stdColours >> op

stdColours :: IO ()
stdColours = setSGR [SetColor Foreground Vivid White]

indented :: (a -> Text) -> [a] -> Text
indented show' = mappend "\t" . T.intercalate "\n\t" . fmap show'

indentedNoLeadingNewline :: (a -> Text) -> a -> Text
indentedNoLeadingNewline show' = mappend "\t" . show'

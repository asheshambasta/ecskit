{-|
Module: Main 
Description: Entrypoint

See the README for more details.

Interact with AWS ECS and manage deployments in a sane way.

-}

module Main
  ( main
  ) where

import qualified Options.Applicative           as A
import           Parse.Conf
import           System.Console.ANSI

main :: IO ()
main = do
  conf <- A.execParser topLevelParse
  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]
  putStrLn @Text (show conf)
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn @Text "Default colors."

topLevelParse = A.info
  (confParse <**> A.helper)
  (A.fullDesc <> A.progDesc "ECSkit" <> A.header "Sane interaction with ECS.")

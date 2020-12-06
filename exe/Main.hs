{-|
Module: Main 
Description: Entrypoint

See the README for more details.

Interact with AWS ECS and manage deployments in a sane way.

-}

module Main
  ( main
  ) where

import           Conf.Runtime
import           Cmd
import qualified Options.Applicative           as A

import           Parse.Conf
import           System.Console.ANSI

import           Polysemy
import           Polysemy.Reader
import           Polysemy.AWS

main :: IO ()
main = do
  conf    <- A.execParser topLevelParse
  runtime <- mkRuntime conf
  result  <- executeWithRuntime runtime
  putStrLn @Text . show $ result
  setSGR [SetColor Foreground Vivid Red]
  setSGR [SetColor Background Vivid Blue]
  putStrLn @Text (show conf)
  setSGR [Reset]  -- Reset to default colour scheme
  putStrLn @Text "Default colors."

topLevelParse :: A.ParserInfo Conf
topLevelParse = A.info
  (confParse <**> A.helper)
  (A.fullDesc <> A.progDesc "ECSkit" <> A.header "Sane interaction with ECS.")

type ExecResult = Either AWSError ()

executeWithRuntime :: Runtime -> IO ExecResult
executeWithRuntime Runtime { _rConf = Conf { _cCmd = AnyCmd cmd, ..}, ..} =
  runM . runError . Polysemy.Reader.runReader _rAWSEnv $ do
    res <- runCmdExplicit cmd
    putStrLn @Text $ show res
    pure ()
  -- $ case _cCmd of
  -- runM . runError . Polysemy.Reader.runReader _rAWSEnv . runCmd $ case _cCmd of
  --   AnyCmd (DescribeServicesCmd ds) -> do
  --     dsr <- describeServicesCmd ds
  --     embed @IO . putStrLn @Text . show $ dsr
  --     pure ()
  --   AnyCmd (DescribeClustersCmd ds) -> do
  --     dsr <- describeClustersCmd ds
  --     embed @IO . putStrLn @Text . show $ dsr
  --     pure ()






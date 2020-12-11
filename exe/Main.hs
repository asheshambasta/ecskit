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
import           Cmd.Disp
import qualified Options.Applicative           as A

import           Parse.Conf

import           Polysemy
import           Polysemy.Reader
import           Polysemy.AWS

main :: IO ()
main = do
  conf    <- A.execParser topLevelParse
  runtime <- mkRuntime conf
  executeWithRuntime runtime $> ()

topLevelParse :: A.ParserInfo Conf
topLevelParse = A.info
  (confParse <**> A.helper)
  (A.fullDesc <> A.progDesc "ECSkit" <> A.header "Sane interaction with ECS.")

type ExecResult = Either AWSError ()

executeWithRuntime :: Runtime -> IO ExecResult
executeWithRuntime Runtime { _rConf = Conf { _cCmd = AnyCmd cmd, ..}, ..} =
  runM . runError . Polysemy.Reader.runReader _rAWSEnv $ do
    res <- runCmdExplicit cmd
    embed $ disp @ 'Terminal res
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


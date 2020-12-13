{-|
Module: Main 
Description: Entrypoint

See the README for more details.

Interact with AWS ECS and manage deployments in a sane way.

-}

module Main
  ( main
  ) where

import           Cmd
import           Cmd.Disp
import           Conf.Runtime
import qualified Options.Applicative           as A

import           Parse.Conf

import           Polysemy
import           Polysemy.AWS
import           Polysemy.Reader

main :: IO ()
main = do
  conf    <- A.execParser topLevelParse
  runtime <- mkRuntime conf
  eRes    <- executeWithRuntime runtime
  case eRes of
    Left  err -> putStrLn @Text (show err) >> exitFailure
    Right _   -> exitSuccess


topLevelParse :: A.ParserInfo Conf
topLevelParse = A.info
  (confParse <**> A.helper)
  (A.fullDesc <> A.progDesc "ECSkit" <> A.header "Sane interaction with ECS.")

type ExecResult = Either AWSError ()

executeWithRuntime :: Runtime -> IO ExecResult
executeWithRuntime Runtime { _rConf = Conf { _cCmd = AnyCmd cmd, ..}, ..} =
  runM . runError . Polysemy.Reader.runReader _rAWSEnv $ do
  -- TODO: determine run-status (exitCode) from the res)
    res <- runCmdExplicit cmd
    embed $ disp @ 'Terminal res
    pure ()

module Parse.Conf
  ( module Conf
  , confParse
  ) where

import           Data.Default.Class             ( def )
import           Parse.Cmd                      ( cmdParse )
import qualified Options.Applicative           as A
import           Conf

confParse :: A.Parser Conf
confParse = Conf defaultCreds <$> parseRegion <*> cmdParse
 where
  parseRegion =
    A.option (A.eitherReader readEither)
      $  A.long "region"
      <> A.short 'R'
      <> A.help "AWS Region"
      <> A.value defaultRegion
      <> A.showDefault
  Conf { _cAWSCredentials = defaultCreds, _cAWSRegion = defaultRegion } = def

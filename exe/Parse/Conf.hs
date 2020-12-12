module Parse.Conf
  ( module Conf
  , confParse
  ) where

import qualified Network.AWS                   as AWS
import           Parse.Cmd                      ( cmdParse )
import qualified Options.Applicative           as A
import           Conf

confParse :: A.Parser Conf
confParse = Conf AWS.Discover <$> parseRegion <*> cmdParse
 where
  parseRegion =
    A.option (A.eitherReader readEither)
      $  A.long "region"
      <> A.short 'R'
      <> A.help "AWS Region"
      <> A.value AWS.Ireland
      <> A.showDefault

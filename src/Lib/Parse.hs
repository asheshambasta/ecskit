{-|
: Lib.Parse
Description: Utilities for parsing.

Some basic primitives for parsing text

-}
module Lib.Parse
  ( module P
  , module Text.Megaparsec.Char
  , ParserText
  , FParseErr
  , ParserTextErr
  , parseFailureText
  , fancyFailure'
  , parse'
  -- * Common parsers
  , parseInt
  , parseInteger
  , parseTextAscii
  , takeUntil1P
  ) where

import           Data.Char                      ( isDigit )
import qualified Data.Set                      as Set
                                                ( singleton )
import           Data.String                    ( String )
import qualified Data.Text                     as T
                                                ( pack
                                                , unpack
                                                )
import           Prelude                 hiding ( many )
import           Text.Megaparsec               as P
import           Text.Megaparsec.Char

type ParserText = Parsec FParseErr Text

newtype FParseErr = GenericParseErr Text
               deriving (Eq, Show, Ord)

-- | Convenient type alias for errors associated with ParserText
type ParserTextErr = ParseErrorBundle Text FParseErr
-- instance TextShow FParseErr where
--   showb = fromString . show

instance ShowErrorComponent FParseErr where
  showErrorComponent = show

parseFailureText :: FParseErr -> ParserText a
parseFailureText = fancyFailure . Set.singleton . ErrorCustom

fancyFailure' :: Text -> ParserText a
fancyFailure' = fancyFailure . Set.singleton . ErrorFail . T.unpack

parseTextAscii :: ParserText Text
parseTextAscii = T.pack <$> many asciiChar

parseInt :: ParserText Int
parseInt = fromIntegral <$> parseInteger

-- | Parse integers tolerantly. This parses the integer upto the point it can parse it; instead of using many
-- which parses and fails if the predicate is not satisfied.
parseInteger :: ParserText Integer
parseInteger = do
  sign <- P.choice [char '-' $> (-1), char '+' $> 1, pure 1]
  either fancyFailure' (pure . (* sign))
    .   first T.pack
    .   readEither
    .   T.unpack
    =<< takeWhile1P (Just "integer:digit") isDigit

parse' :: ParserText a -> Text -> Either (ParseErrorBundle Text FParseErr) a
parse' p src = parse p env' src where env' = T.unpack src

takeUntil1P :: Maybe String -> Char -> ParserText Text
takeUntil1P mname c = takeWhile1P mname (/= c)

module Utils.Parsing where

import Prelude
import Data.CodePoint.Unicode (isAlpha)
import Parsing.String.Basic (digit, takeWhile)
import Parsing (Parser, fail)
import Parsing.Combinators (sepBy, tryRethrow)
import Parsing.String (char)
import Data.List.Types (List)
import Data.String.CodeUnits (fromCharArray) as String
import Data.Int (fromString) as Int
import Data.Maybe (maybe)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Parsing.Combinators.Array (many1)
import Control.Alt ((<|>))
import Data.Array.NonEmpty (toArray) as NA
import Parsing.Combinators (skipMany1) as P

linesOf :: forall a. Parser String a -> Parser String (List a)
linesOf p = p `sepBy` char '\n'

word :: Parser String String
word = takeWhile isAlpha

spaces :: Parser String Unit
spaces = unit <$ P.skipMany1 (char ' ')

intDigit :: Parser String Int
intDigit = do
  c <- digit
  maybe (fail "Expected digit") pure $ Int.fromString $ String.fromCharArray [c]

bigInt :: Parser String BigInt
bigInt = tryRethrow do
  digits <- many1 digit <|> fail "Expected BigInt"
  maybe (fail "Expected BigInt") pure
    $ BigInt.fromString
    $ String.fromCharArray $ NA.toArray digits

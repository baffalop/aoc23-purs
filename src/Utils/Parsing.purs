module Utils.Parsing where

import Prelude
import Data.CodePoint.Unicode (isAlpha)
import Parsing.String.Basic (digit, takeWhile)
import Parsing (Parser, fail)
import Parsing.Combinators (sepBy)
import Parsing.String (char)
import Data.List.Types (List)
import Data.String.CodeUnits (fromCharArray) as String
import Data.Int (fromString) as Int
import Data.Maybe (maybe)

linesOf :: forall a. Parser String a -> Parser String (List a)
linesOf p = p `sepBy` char '\n'

word :: Parser String String
word = takeWhile isAlpha

intDigit :: Parser String Int
intDigit = do
  c <- digit
  maybe (fail "Expected digit") pure $ Int.fromString $ String.fromCharArray [c]

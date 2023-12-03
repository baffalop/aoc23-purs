module Utils.Parsing where

import Prelude
import Data.CodePoint.Unicode (isAlpha)
import Parsing.String.Basic (takeWhile)
import Parsing (Parser)
import Parsing.Combinators (sepBy)
import Parsing.String (char)
import Data.List.Types (List)

linesOf :: forall a. Parser String a -> Parser String (List a)
linesOf p = p `sepBy` char '\n'

word :: Parser String String
word = takeWhile isAlpha


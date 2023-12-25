module Utils.Parsing where

import Prelude
import Data.CodePoint.Unicode (isAlpha, isAlphaNum)
import Parsing.String.Basic (digit, takeWhile)
import Parsing (Parser, fail)
import Parsing.Combinators (sepBy, tryRethrow)
import Parsing.String (char)
import Data.List.Types (List)
import Data.String.CodeUnits (fromCharArray) as String
import Data.Int (fromString) as Int
import Data.Maybe (Maybe, maybe)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Parsing.Combinators.Array (many1)
import Control.Alt ((<|>))
import Data.Array.NonEmpty (toArray) as NA
import Parsing.Combinators (skipMany1) as P
import Parsing.Combinators.Array (many) as PA
import Data.Array (cons) as Array

fromMaybe :: forall a. String -> Maybe a -> Parser String a
fromMaybe failureMessage = maybe (fail failureMessage) pure

listLinesOf :: forall a. Parser String a -> Parser String (List a)
listLinesOf p = p `sepBy` char '\n'

linesOf :: forall a. Parser String a -> Parser String (Array a)
linesOf p = p `arraySepBy` char '\n'

arraySepBy :: forall a b. Parser String a -> Parser String b -> Parser String (Array a)
arraySepBy p sep = Array.cons <$> p <*> PA.many (sep *> p)

word :: Parser String String
word = takeWhile isAlpha

wordAlphaNum :: Parser String String
wordAlphaNum = takeWhile isAlphaNum

spaces :: Parser String Unit
spaces = unit <$ P.skipMany1 (char ' ')

intDigit :: Parser String Int
intDigit = do
  c <- digit
  fromMaybe "Expected digit" $ Int.fromString $ String.fromCharArray [c]

bigInt :: Parser String BigInt
bigInt = tryRethrow do
  digits <- many1 digit <|> fail "Expected BigInt"
  fromMaybe "Expected BigInt" $ BigInt.fromString $ String.fromCharArray $ NA.toArray digits

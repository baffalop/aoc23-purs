module Day01 where

import Prelude
import Utils.String (lines)
import Data.Traversable (traverse)
import Parsing (ParseError, runParser)
import Parsing.Combinators.Array (many) as P
import Parsing.String (anyTill) as P
import Parsing.String.Basic (digit) as P
import Data.Tuple (snd) as Tuple
import Data.Either (Either)

parse :: String -> Either ParseError (Array (Array Char))
parse = lines >>> traverse (flip runParser digits)
  where
    digits = P.many $ Tuple.snd <$> P.anyTill P.digit

example1 = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

example2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

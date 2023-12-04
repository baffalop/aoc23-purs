module Day01 where

import Prelude
import Utils.String (lines)
import Data.Traversable (traverse)
import Parsing (ParseError, runParser)
import Parsing.Combinators.Array (many1) as P
import Parsing.String (anyTill) as P
import Data.Tuple (snd) as Tuple
import Data.Either (Either)
import Utils.Parsing (intDigit)
import Utils.Pointfree ((<<$>>))
import Data.Array as Array
import Data.Foldable (sum) as F
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Input (readInput)

solve1 :: String -> Either ParseError Int
solve1 = F.sum <<< map calibration <<$>> parse
  where
    calibration digits = (NA.head digits * 10) + (NA.last digits)

parse :: String -> Either ParseError (Array (NonEmptyArray Int))
parse = lines >>> traverse (flip runParser digits)
  where
    digits = P.many1 $ Tuple.snd <$> P.anyTill intDigit

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

input = readInput 1

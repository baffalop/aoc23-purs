module Day01 where

import Prelude
import Utils.String (lines)
import Data.Traversable (traverse)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators.Array (many1) as P
import Parsing.String (anyTill, string, takeN) as P
import Data.Tuple (snd) as Tuple
import Data.Either (Either)
import Utils.Parsing (intDigit)
import Utils.Pointfree ((<<$>>))
import Data.Array as Array
import Data.Foldable (sum) as F
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Input (readInput)
import Parsing.Combinators (choice, lookAhead) as P
import Control.Alt ((<|>))

type Parse = String -> Either ParseError (Array (NonEmptyArray Int))

solve1 :: String -> Either ParseError Int
solve1 = solve parse1

solve2 :: String -> Either ParseError Int
solve2 = solve parse2

solve :: Parse -> String -> Either ParseError Int
solve parse = F.sum <<< map calibration <<$>> parse
  where
    calibration digits = (NA.head digits * 10) + (NA.last digits)

parse1 :: Parse
parse1 = parseWith intDigit

parse2 :: Parse
parse2 = parseWith $ P.lookAhead (intDigit <|> wordDigit) <* P.takeN 1
  where
    wordDigit = P.choice
      [ 1 <$ P.string "one"
      , 2 <$ P.string "two"
      , 3 <$ P.string "three"
      , 4 <$ P.string "four"
      , 5 <$ P.string "five"
      , 6 <$ P.string "six"
      , 7 <$ P.string "seven"
      , 8 <$ P.string "eight"
      , 9 <$ P.string "nine"
      ]

parseWith :: Parser String Int -> Parse
parseWith digit =
  lines >>> traverse (flip runParser $ P.many1 $ Tuple.snd <$> P.anyTill digit)

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

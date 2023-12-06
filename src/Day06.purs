module Day06 where

import Prelude
import Input (readInput)
import Parsing (ParseError, runParser)
import Parsing.String (char, string) as P
import Parsing.String.Basic (skipSpaces) as P
import Utils.Parsing (bigInt, spaces) as P
import Parsing.Combinators (sepBy)
import Data.List as List
import Data.Either (Either)
import Data.List (List)
import Utils.Pointfree ((<<#>>))
import Data.Foldable (product) as F
import Data.String.Common (replaceAll) as String
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt

type Race =
  { time :: BigInt
  , distance :: BigInt
  }

solve1 :: String -> Either ParseError BigInt
solve1 = parse <<#>> map countMoves >>> F.product
  where
    countMoves race@{ time } = time - (firstWinningMove race * BigInt.fromInt 2) + BigInt.fromInt 1

    firstWinningMove { time, distance } = firstFrom $ BigInt.fromInt 1
      where firstFrom n = if n * (time - n) > distance then n else firstFrom $ n + BigInt.fromInt 1

solve2 :: String -> Either ParseError BigInt
solve2 = String.replaceAll (Pattern " ") (Replacement "") >>> solve1

parse :: String -> Either ParseError (List Race)
parse s = runParser s do
  times <- P.string "Time:" *> P.skipSpaces *> ints
  _ <- P.char '\n'
  distances <- P.string "Distance:" *> P.skipSpaces *> ints
  pure $ List.zipWith { time: _, distance: _ } times distances
  where
    ints = P.bigInt `sepBy` P.spaces

example = "Time:      7  15   30\nDistance:  9  40  200"

input = readInput 6

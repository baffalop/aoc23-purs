module Day06 where

import Prelude
import Input (readInput)
import Parsing (ParseError, runParser)
import Parsing.String (char, string) as P
import Parsing.String.Basic (intDecimal) as P
import Utils.Parsing (spaces) as P
import Parsing.Combinators (sepBy)
import Data.List as List
import Data.Either (Either)
import Data.List (List)

type Race =
  { time :: Int
  , distance :: Int
  }

parse :: String -> Either ParseError (List Race)
parse s = runParser s do
  times <- P.string "Time:" *> P.spaces *> ints
  _ <- P.char '\n'
  distances <- P.string "Distance:" *> P.spaces *> ints
  pure $ List.zipWith { time: _, distance: _ } times distances
  where
    ints = P.intDecimal `sepBy` P.spaces

example = """Time:      7  15   30
Distance:  9  40  200"""

input = readInput 6

module Day02 where

import Prelude
import Parsing (ParseError, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String as P
import Parsing.String.Basic as P
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))
import Utils.Parsing as UP
import Data.List.Types (List(..))
import Data.Either (Either)
import Data.List as L
import Data.Foldable as F
import Data.Ord (max)
import Control.Apply (lift2)
import Utils.Pointfree ((<<#>>))
import Input (readInput)

type Game =
  { id :: Int
  , rounds :: List Round
  }

type Round = Map String Int

solve1 = parse
  <<#>> L.filter (_.rounds >>> F.all belowLimits)
  >>> map _.id
  >>> F.sum
  where
    belowLimits :: Round -> Boolean
    belowLimits colours = F.and $ (<=) <$> colours <*> limits

    limits = Map.fromFoldable
      [ Tuple "red" 12
      , Tuple "green" 13
      , Tuple "blue" 14
    ]

solve2 = parse
  <<#>> map (_.rounds >>> F.foldr (lift2 max) zero >>> F.product)
  >>> F.sum

parse :: String -> Either ParseError (List Game)
parse s = runParser s $ UP.listLinesOf game
  where
    game = do
      id <- P.string "Game " *> P.intDecimal
      rounds <- P.string ": " *> round `sepBy` P.string "; "
      pure { id, rounds }

    round = do
     cubes <- cube `sepBy` P.string ", "
     pure $ Map.union (Map.fromFoldable cubes) zero

    cube = do
      n <- P.intDecimal
      colour <- P.string " " *> UP.word
      pure $ Tuple colour n

zero :: Round
zero = Map.fromFoldable $ map (\c -> Tuple c 0) ["red", "green", "blue"]

example :: String
example = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

input = readInput 2

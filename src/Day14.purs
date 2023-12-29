module Day14 where
-- Parabolic Reflector Dish

import Prelude
import Utils.String (lines) as String
import Data.String.CodeUnits (toCharArray) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Array (catMaybes) as Array

data Rock = Round | Square

type Coord = Tuple Int Int

derive instance eqRock :: Eq Rock
instance showRock :: Show Rock where
  show Round = "O"
  show Square = "#"

parse :: String -> Map Coord Rock
parse = String.lines
  >>> mapWithIndex (\row ->
    String.toCharArray >>> mapWithIndex (\col c ->
      Tuple (col /\ row) <$> case c of
        '#' -> Just Square
        'O' -> Just Round
        _ -> Nothing
    ) >>> Array.catMaybes
  )
  >>> join
  >>> Map.fromFoldable

example = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""

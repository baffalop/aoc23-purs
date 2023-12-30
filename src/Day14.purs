module Day14 where
-- Parabolic Reflector Dish

import Prelude
import Utils.String (lines) as String
import Data.String.CodeUnits (toCharArray) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), maybe)
import Data.Array (catMaybes) as Array
import Data.Foldable as F
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Set as Set
import Input (readInput)

data Rock = Round | Square

type Coord = Tuple Int Int

derive instance eqRock :: Eq Rock
instance showRock :: Show Rock where
  show Round = "O"
  show Square = "#"

solve1 :: String -> Int
solve1 = parse >>> tiltRocks >>> totalLoad
  where
    tiltRocks :: Map Coord Rock -> Map Coord Rock
    tiltRocks rocks = foldlWithIndex
      (\coord@(x /\ y) { rocks, lastCoord: (lastx /\ lasty) } rock -> case rock of
        Square -> { rocks, lastCoord: coord }
        Round ->
          let movedCoord = if x == lastx then x /\ (lasty + 1) else x /\ 0
          in
          { rocks: rocks # Map.delete coord # Map.insert movedCoord rock
          , lastCoord: movedCoord
          }
      )
      { lastCoord: 0 /\ -1, rocks }
      rocks
      # _.rocks

    totalLoad :: Map Coord Rock -> Int
    totalLoad rocks = F.sum
      $ mapWithIndex (\(_ /\ y) -> case _ of
        Square -> 0
        Round -> rows - y
      ) rocks
      where
        rows = Map.keys rocks # Set.map snd # Set.findMax # maybe 0 (_ + 1)

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

input = readInput 14

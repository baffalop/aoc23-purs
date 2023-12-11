module Day11 where
-- Cosmic Expansion

import Prelude
import Utils.String (allIndicesOf, lines) as String
import Data.String.Pattern (Pattern(Pattern))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Tuple (Tuple, Tuple(Tuple), fst, snd, uncurry)
import Data.Tuple.Nested ((/\))
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Array ((..))
import Utils.Basics (mapBoth)
import Data.Bifunctor (bimap)
import Data.Map.Internal (Map)
import Data.Map.Internal as Map
import Data.Foldable as F
import Data.Ord (abs)
import Input (readInput)

type Coord = Tuple Int Int

solve1 :: String -> Int
solve1 s =
  let
    galaxies = parse s
    populatedCols /\ populatedRows = mapBoth (\get -> Set.fromFoldable $ get <$> galaxies) $ fst /\ snd
    maxX /\ maxY = mapBoth (fromMaybe 0 <<< Set.findMax) $ populatedCols /\ populatedRows
    emptyRows = Set.fromFoldable $ Array.filter (\x -> not $ Set.member x populatedRows) (0 .. maxX)
    emptyCols = Set.fromFoldable $ Array.filter (\y -> not $ Set.member y populatedCols) (0 .. maxY)
    expandedGalaxies = bimap (expandBy $ Set.toMap emptyCols) (expandBy $ Set.toMap emptyRows) <$> galaxies
  in
  F.sum $ uncurry manhattanDistance <$> pairs expandedGalaxies
  where
    expandBy :: Map Int Unit -> Int -> Int
    expandBy empty coord = coord + Map.size (Map.submap Nothing (Just $ coord - 1) empty)

    pairs :: Array Coord -> Array (Tuple Coord Coord)
    pairs coords = Array.nub $ do
      c1 <- coords
      c2 <- coords
      if c1 == c2 then [] else [Tuple (min c1 c2) (max c1 c2)]

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance c1 c2 = uncurry (+) $ mapBoth abs $ c1 - c2

parse :: String -> Array Coord
parse = String.lines
  >>> map (String.allIndicesOf $ Pattern "#")
  >>> foldrWithIndex (\y xs coords -> coords <> ((_ /\ y) <$> xs)) []

example = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""

input = readInput 11

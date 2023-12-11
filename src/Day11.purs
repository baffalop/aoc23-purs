module Day11 where
-- Cosmic Expansion

import Prelude
import Utils.String (allIndicesOf, lines) as String
import Data.String.Pattern (Pattern(Pattern))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Tuple (Tuple(Tuple), fst, snd, uncurry)
import Data.Tuple.Nested ((/\))
import Data.Set as Set
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
import Data.BigInt (BigInt)
import Data.BigInt as BigInt

type Coord a = Tuple a a

solve1 :: String -> BigInt
solve1 = solveWith 2

solve2 :: String -> BigInt
solve2 = solveWith 1_000_000

solveWith :: Int -> String -> BigInt
solveWith expansionFactor s =
  let
    galaxies = parse s
    populatedCols = Set.fromFoldable $ fst <$> galaxies
    populatedRows = Set.fromFoldable $ snd <$> galaxies
    maxX /\ maxY = mapBoth (fromMaybe 0 <<< Set.findMax) $ populatedCols /\ populatedRows
    emptyRows = Set.fromFoldable $ Array.filter (\x -> not $ Set.member x populatedRows) (0 .. maxX)
    emptyCols = Set.fromFoldable $ Array.filter (\y -> not $ Set.member y populatedCols) (0 .. maxY)
    expandedGalaxies = bimap (expandBy $ Set.toMap emptyCols) (expandBy $ Set.toMap emptyRows) <$> galaxies
  in
  F.sum $ uncurry manhattanDistance <$> pairs expandedGalaxies
  where
    expandBy :: Map Int Unit -> Int -> BigInt
    expandBy empty coord =
      let emptyBefore = Map.size (Map.submap Nothing (Just $ coord - 1) empty)
      in BigInt.fromInt $ coord + emptyBefore * (expansionFactor - 1)

    pairs coords = Array.nub $ do
      c1 <- coords
      c2 <- coords
      if c1 == c2 then [] else [Tuple (min c1 c2) (max c1 c2)]

manhattanDistance :: forall a. Ord a => Ring a => Coord a -> Coord a -> a
manhattanDistance c1 c2 = uncurry (+) $ mapBoth abs $ c1 - c2

parse :: String -> Array (Coord Int)
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

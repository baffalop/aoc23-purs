module Day13 where
-- Point of Incidence

import Prelude
import Input (readInput)
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(Pattern)) as S
import Parsing (ParseError, runParser)
import Parsing.String (char) as P
import Utils.Parsing (linesOf) as P
import Data.Traversable (traverse)
import Parsing.Combinators.Array (many) as P
import Parsing.Combinators (choice) as P
import Matrix as Mx
import Matrix (Matrix)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Parsing (fail) as P
import Data.Either (Either)
import Utils.Pointfree ((<<#>>))
import Data.Map.Internal (fromFoldableWith) as Map
import Data.Array as Array
import Data.Tuple (Tuple(Tuple))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import PointFree ((<..))
import Data.Set as Set
import Data.Foldable as F
import Data.Set (Set)
import Data.Array ((..))
import Control.Alt ((<|>))

data Terrain = Ash | Rock | Refl -- Refl for debugging purposes

derive instance eqTerrain :: Eq Terrain
derive instance ordTerrain :: Ord Terrain
instance showTerrain :: Show Terrain where
  show Ash = "."
  show Rock = "#"
  show Refl = "@"

type Pattern = Matrix Terrain

solve1 :: String -> Either ParseError Int
solve1 = parse
  <<#>> map (\pattern -> 100 * findReflection (Mx.rows pattern) + findReflection (Mx.columns pattern))
  >>> F.sum

solve2 :: String -> Either ParseError Int
solve2 = parse
  <<#>> map (\pattern ->
    fromMaybe 0 $ (100 * _) <$> findSmudged (Mx.rows pattern) <|> findSmudged (Mx.columns pattern)
  ) >>> F.sum
  where
    findSmudged slices = F.findMap reflectionPoint oneOffPairs
      where
        bound = Array.length slices - 1
        matchedPairs = matchPairs slices
        oneOffPairs = Array.mapWithIndex { i: _, v: _ } slices
          # pairs
          # Array.mapMaybe \({ i: i1, v: v1 } /\ { i: i2, v: v2 }) -> case differences v1 v2 of
            [_] -> Just (i1 /\ i2)
            _ -> Nothing

        reflectionPoint pair@(i1 /\ i2)
          -- a pair of indices whose difference is even doesn't actually lie on either side of a reflection point
          | (i2 - i1) `mod` 2 == 0 = Nothing
          | otherwise =
            let refl = ((i2 - i1) `div` 2) + i1
            in if F.all (\p -> p == pair || p `Set.member` matchedPairs) $ Array.zip (refl .. 0) ((refl + 1) .. bound)
              then Just (refl + 1) else Nothing

differences :: forall a. Eq a => Array a -> Array a -> Array Int
differences = (Array.catMaybes <<< Array.mapWithIndex \i t -> if t then Just i else Nothing) <.. Array.zipWith (/=)

findReflection :: Array (Array Terrain) -> Int
findReflection slices = fromMaybe 0 $ F.findMap reflectionPoint matchedPairs
  where
    bound = Array.length slices - 1
    matchedPairs = matchPairs slices
    reflectionPoint (Tuple i1 i2)
      | i1 == 0 || i2 == bound =
        let refl = ((i2 - i1) `div` 2) + i1
        in if F.all (_ `Set.member` matchedPairs) $ Array.zip ((i1 + 1) .. refl) ((i2 - 1) .. (refl + 1))
          then Just $ refl + 1 else Nothing
      | otherwise = Nothing

debugReflections :: Maybe Int -> Maybe Int -> Matrix Terrain -> Matrix Terrain
debugReflections colRefl rowRefl = Mx.indexedMap \col row ter ->
  if ter == Rock && (isColReflected col || isRowReflected row) then Refl else ter
  where
    isColReflected = maybe (const false) (<=) colRefl
    isRowReflected = maybe (const false) (<=) rowRefl

matchPairs :: forall a. Ord a => Array a -> Set (Tuple Int Int)
matchPairs = Array.mapWithIndex (\i s -> Tuple s [i])
  >>> Map.fromFoldableWith (Array.sort <.. (<>))
  >>> (Array.fromFoldable >=> pairs)
  >>> Set.fromFoldable

pairs :: forall a. Array a -> Array (Tuple a a)
pairs xs = join $ Array.mapWithIndex (\i x -> Tuple x <$> Array.drop (i + 1) xs) xs

parse :: String -> Either ParseError (Array Pattern)
parse = String.split (S.Pattern "\n\n") >>> traverse (flip runParser pattern)
  where
    pattern = do
      ar <- P.linesOf $ P.many $ P.choice
        [ Ash <$ P.char '.'
        , Rock <$ P.char '#'
        ]
      maybe (P.fail "Can't build Matrix") pure $ Mx.fromArray ar

example = """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""

faultyExample = """..####...#.#..#
########.......
#......##.##.##
##....##..#.##.
.#.##.#.....###
#.####.###.####
#.####.#####..#
##....###.#.##.
........###.###
#########...#..
#########......

####..###...###
######.....##..
######.#.#..#.#
######.##.#.##.
#####....#...#.
.....#.##.###..
....#..####.##.
#######.##.....
#####.##.#.#.#.
......#..#..#..
####.#..#.##...
....#....#....#
..........##...
####..###....#.
#..#####.#.....

.#..##....#.###
#..##.#..##.#..
##.##.#..##.#..
.#..##....#.###
###.###.##.#..#
.#..##.##..###.
##.#.##.####...
##.#.##.####...
.#..##.##..###.
###.###.##.#..#
.#..##....#.###
##.##.#..##.#..
#..##.#..##.#..
.#..##....#.###
#.#..###..###.#
#.###.#..#.##.#
.##..#.#...#..#"""

input = readInput 13

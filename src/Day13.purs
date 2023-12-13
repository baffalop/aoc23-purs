module Day13 where

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
import Data.Array ((..))
import Data.Tuple (Tuple(Tuple))
import Data.Tuple as Tuple
import PointFree ((<..))
import Data.Set as Set
import Data.Foldable as F

data Terrain = Ash | Rock

derive instance eqTerrain :: Eq Terrain
derive instance ordTerrain :: Ord Terrain
instance showTerrain :: Show Terrain where
  show Ash = "."
  show Rock = "#"

type Pattern = Matrix Terrain

solve1 :: String -> Either ParseError Int
solve1 = parse
  <<#>> map (\p -> 100 * findReflection (Mx.rows p) + findReflection (Mx.columns p))
  >>> F.sum

findReflection :: Array (Array Terrain) -> Int
findReflection slices = fromMaybe 0 $ F.findMap reflectionPoint reflectedPairs
  where
    bound = Array.length slices - 1

    reflectedPairs = Array.mapWithIndex (\i s -> Tuple s [i]) slices
      # Map.fromFoldableWith (Array.sort <.. (<>))
      # (Array.fromFoldable >=> pairs)
      # Set.fromFoldable

    reflectionPoint (Tuple i1 i2)
      | i1 == 0 || i2 == bound =
        let refl = ((i2 - i1) `div` 2) + i1
        in if F.all (\i -> (Tuple i $ i1 + i2 - i) `Set.member` reflectedPairs) ((i1 + 1) .. refl)
          then Just $ refl + 1 else Nothing
      | otherwise = Nothing

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

input = readInput 13

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
import Matrix as Matrix
import Matrix (Matrix)
import Data.Maybe (maybe)
import Parsing (fail) as P
import Data.Either (Either)

data Terrain = Ash | Rock

derive instance eqTerrain :: Eq Terrain
derive instance ordTerrain :: Ord Terrain
instance showTerrain :: Show Terrain where
  show Ash = "."
  show Rock = "#"

type Pattern = Matrix Terrain

parse :: String -> Either ParseError (Array Pattern)
parse = String.split (S.Pattern "\n\n") >>> traverse (flip runParser pattern)
  where
    pattern = do
      ar <- P.linesOf $ P.many $ P.choice
        [ Ash <$ P.char '.'
        , Rock <$ P.char '#'
        ]
      maybe (P.fail "Can't build Matrix") pure $ Matrix.fromArray ar

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

module Day10 where
-- Pipe Maze

import Prelude
import Parsing (ParseError, Position(Position), runParser)
import Utils.Parsing (linesOf)
import Data.Map (Map)
import Data.Map as Map
import Utils.Pointfree ((<<$>>))
import Parsing.Combinators.Array (many) as P
import Data.Tuple.Nested ((/\), type (/\))
import Parsing.Combinators (choice) as P
import Parsing.String (char) as P
import Parsing (position) as P
import Data.Tuple (Tuple(Tuple))
import Control.Bind (join)
import Data.Either (Either)

type Coord = Int /\ Int

parse :: String -> Either ParseError (Map Coord (Array Coord))
parse s = runParser s $ Map.fromFoldable <<< join <<$>> linesOf $ P.many do
  openings <- P.choice
    [ [] <$ P.char 'S'
    , [north, south] <$ P.char '|'
    , [east, west] <$ P.char '-'
    , [west, south] <$ P.char '7'
    , [west, north] <$ P.char 'J'
    , [east, south] <$ P.char 'F'
    , [east, north] <$ P.char 'L'
    ]
  Position { line, column } <- P.position
  let coord = column /\ line
  pure $ Tuple coord $ (coord + _) <$> openings

north = 0 /\ -1
south = 0 /\ 1
east = 1 /\ 0
west = -1 /\ 0

example2 = """7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"""

module Day10 where
-- Pipe Maze

import Prelude
import Parsing (ParseError, Position(Position), parseErrorMessage, runParser)
import Utils.Parsing (linesOf)
import Data.Map (Map)
import Data.Map as Map
import Utils.Pointfree ((<<$>>))
import Parsing.Combinators.Array (many) as P
import Data.Tuple.Nested ((/\))
import Parsing.Combinators (choice, skipMany) as P
import Parsing.String (char) as P
import Parsing (position) as P
import Data.Tuple (Tuple(Tuple), fst, snd)
import Control.Bind (join)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Either (note) as Either
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Array.NonEmpty (NonEmptyArray, (:))
import Data.Array.NonEmpty as NA
import Data.Foldable as F
import Input (readInput)

type Coord = Tuple Int Int

solve1 :: String -> Either String Int
solve1 s = do
  pipes <- lmap parseErrorMessage $ parse s
  start <- Either.note "No start found" $ fst <<$>> Array.find (snd >>> Array.null) $ Map.toUnfoldable pipes
  let
    follow :: NonEmptyArray Coord -> Coord -> Maybe (NonEmptyArray Coord)
    follow visited to
      | to == start = Just visited
      | otherwise = case Array.filter (_ /= NA.head visited) <$> Map.lookup to pipes of
        Just [next] -> follow (to : visited) next
        _ -> Nothing

  route <- Either.note "No route found" $ F.oneOf $ follow (NA.singleton start) <$> neighbours start
  pure $ NA.length route `div` 2

neighbours :: Coord -> Array Coord
neighbours c = (c + _) <$> [north, south, east, west]

parse :: String -> Either ParseError (Map Coord (Array Coord))
parse s = runParser s $ Map.fromFoldable <<< join <<$>> linesOf $ P.many do
  dot
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
  dot
  let coord = column /\ line
  pure $ Tuple coord $ (coord + _) <$> openings
  where
    dot = P.skipMany $ P.char '.'

north = 0 /\ -1
south = 0 /\ 1
east = 1 /\ 0
west = -1 /\ 0

example2 = """7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"""

input = readInput 10

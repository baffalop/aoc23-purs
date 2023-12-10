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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (note) as Either
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Foldable as F
import Input (readInput)
import Data.Set (Set)
import Data.Set as Set
import Utils.String (lines) as String
import Data.Ord (between)
import Data.String (length) as String
import Control.Monad.State.Class (class MonadState)
import Data.Foldable (class Foldable)
import Uncurried.State (State, runState)
import Data.Lens.Record (prop) as L
import Type.Proxy (Proxy(Proxy))
import Data.Lens.Setter ((%~))
import Control.Monad.State.Class (modify) as State
import Data.Traversable (traverse)
import Data.List (List(..), (:))
import Data.List as List
import Data.Lens.Getter (use)

type Coord = Tuple Int Int

solve1 :: String -> Either String Int
solve1 s = do
  pipes <- lmap parseErrorMessage $ parse s
  loop <- findLoop pipes
  pure $ Set.size loop `div` 2

solve2 :: String -> Either String Int
solve2 s = do
  pipes <- lmap parseErrorMessage $ parse s
  loop <- findLoop pipes
  pure $ Set.size $ foldrState fillFrom { visited: loop } Set.empty loop
  where
    lines = String.lines s
    colCount = fromMaybe 0 $ String.length <$> Array.head lines
    outOfBounds = not <<< between (1 /\ 1) (colCount /\ Array.length lines)

    fillFrom :: Coord -> Set Coord -> State { visited :: Set Coord } (Set Coord)
    fillFrom coord enclosed = do
      branches <- traverse (fill Set.empty <<< List.singleton) $ neighbours coord
      pure $ Set.union enclosed $ F.fold $ List.catMaybes branches

    fill :: Set Coord -> List Coord -> State { visited :: Set Coord } (Maybe (Set Coord))
    fill enclosed Nil = pure $ Just enclosed
    fill enclosed (coord : candidates)
      | outOfBounds coord = pure Nothing
      | otherwise = do
        alreadyVisited <- Set.member coord <$> use _visited
        if alreadyVisited then fill enclosed candidates else do
          { visited } <- State.modify $ _visited %~ Set.insert coord
          fill (Set.insert coord enclosed)
            $ List.filter (not <<< (_ `Set.member` visited))
            $ candidates <> neighbours coord

findLoop :: Map Coord (Array Coord) -> Either String (Set Coord)
findLoop pipes = do
  start <- Either.note "No start found" $ fst <<$>> Array.find (Array.null <<< snd) $ Map.toUnfoldable pipes
  let
    follow :: NonEmptyArray Coord -> Coord -> Maybe (Set Coord)
    follow visited to
      | to == start = Just $ Set.fromFoldable visited
      | otherwise = case Array.filter (_ /= NA.head visited) <$> Map.lookup to pipes of
        Just [next] -> follow (to NA.: visited) next
        _ -> Nothing

  Either.note "No loop found" $ F.oneOf $ follow (NA.singleton start) <$> neighbours start

foldrState :: forall f s a b. Foldable f => (a -> b -> State s b) -> s -> b -> f a -> b
foldrState f initS initV = fst <<< F.foldr (\v (r /\ s) -> runState s $ f v r) (initV /\ initS)

neighbours :: Coord -> List Coord
neighbours c = (c + _) <$> (north : south : east : west : Nil)

_visited = L.prop (Proxy :: Proxy "visited")

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

example1 = """7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"""

example2 = """.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J."""

example3 = """.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."""

example4 = """FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"""

input = readInput 10

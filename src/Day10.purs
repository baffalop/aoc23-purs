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
import Data.Tuple (Tuple(Tuple), fst, snd, uncurry)
import Control.Bind (join)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (note) as Either
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NA
import Data.Foldable as F
import Input (readInput)
import Data.Set (Set)
import Data.Set as Set
import Utils.String (lines) as String
import Data.Ord (abs, between)
import Data.String (length) as String
import Data.Foldable (class Foldable)
import Uncurried.State (State)
import Data.Lens.Record (prop) as L
import Type.Proxy (Proxy(Proxy))
import Data.Lens.Setter ((%~), (.=))
import Control.Monad.State.Class (modify) as State
import Data.List (List(..), (:), (\\))
import Data.List as List
import Data.Lens.Getter (use)
import Utils.State (foldrState)
import Control.Applicative (pure)
import Utils.Basics (mapBoth)
import Debug (spy, spyWith) as Debug

type Coord = Tuple Int Int

type FillState =
  { visited :: Set Coord
  , from :: List Coord
  }

solve1 :: String -> Either String Int
solve1 s = do
  pipes <- lmap parseErrorMessage $ parse s
  loop <- findLoop pipes
  pure $ NA.length loop `div` 2

solve2 s = do
  rawPipes <- lmap parseErrorMessage $ parse s
  loop <- findLoop rawPipes
  pipes <- Either.note "Can't fill starting pipe" do
    let start = NA.last loop
    let end = NA.head loop
    prev <- loop !! (NA.length loop - 2)
    pure $ Map.insert start [prev, end] rawPipes

  let
    surroundings :: Coord -> Maybe (List Coord)
    surroundings coord = do
      passages <- Map.lookup coord pipes
      let continuations = join $ Array.mapMaybe (_ `Map.lookup` pipes) passages
      pure $ diagonalNeighbours coord \\ List.fromFoldable (passages <> continuations)

    walk :: Coord -> Maybe (Set Coord) -> State FillState (Maybe (Set Coord))
    walk _ Nothing = pure Nothing
    walk coord (Just enclosed) = case surroundings coord of
      Nothing -> pure Nothing
      Just candidates -> do
        from <- use _from
        let
          _ = Debug.spyWith "coord" show coord
          _ = Debug.spyWith "from" show from
          toFill = Debug.spyWith "to fill" show $ contiguousWith from candidates
        _from .= toFill
        filled <- fill enclosed $ List.fromFoldable toFill
        pure $ Debug.spyWith "filled" show filled

    fill :: Set Coord -> List Coord -> State FillState (Maybe (Set Coord))
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

  startingPoints <- Either.note "Can't prepare starting points" $ surroundings $ NA.head loop

  pure $ F.findMap
    (\start -> foldrState walk { from: start : Nil, visited: Set.fromFoldable loop } (Just Set.empty) loop)
    startingPoints
  where
    lines = String.lines s
    colCount = fromMaybe 0 $ String.length <$> Array.head lines
    outOfBounds = not <<< between (1 /\ 1) (colCount /\ Array.length lines)

findLoop :: Map Coord (Array Coord) -> Either String (NonEmptyArray Coord)
findLoop pipes = do
  start <- Either.note "No start found" $ fst <<$>> Array.find (Array.null <<< snd) $ Map.toUnfoldable pipes
  let
    follow :: NonEmptyArray Coord -> Coord -> Maybe (NonEmptyArray Coord)
    follow visited to
      | to == start = Just visited
      | otherwise = case Array.filter (_ /= NA.head visited) <$> Map.lookup to pipes of
        Just [next] -> follow (to NA.: visited) next
        _ -> Nothing

  Either.note "No loop found" $ F.oneOf $ follow (NA.singleton start) <$> neighbours start

neighbours :: Coord -> List Coord
neighbours c = (c + _) <$> (north : south : east : west : Nil)

diagonalNeighbours :: Coord -> List Coord
diagonalNeighbours c =
  (c + _) <$> (north : (north + east) : east : (east + south) : south : (south + west) : west : (west + north) : Nil)

contiguousWith :: List Coord -> List Coord -> List Coord
contiguousWith neighbouring coords =
  case List.partition (\c -> F.any (adjacent c) neighbouring) coords of
    { yes: Nil } -> Nil
    { yes, no } -> yes <> contiguousWith (List.nub $ yes <> neighbouring) no

adjacent :: Coord -> Coord -> Boolean
adjacent c1 c2 = uncurry (&&) $ mapBoth (abs >>> (_ <= 1)) $ c1 - c2

_visited = L.prop (Proxy :: Proxy "visited")
_from = L.prop (Proxy :: Proxy "from")

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

example2 = """.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J."""

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

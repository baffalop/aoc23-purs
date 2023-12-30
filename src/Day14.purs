module Day14 where
-- Parabolic Reflector Dish

import Prelude
import Utils.String (lines) as String
import Data.String.CodeUnits (toCharArray) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Foldable as F
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Set as Set
import Input (readInput)
import Data.Lens.Lens.Tuple (_1, _2)
import Data.Lens.Types (Lens')
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((.~))
import Data.Foldable (class Foldable)

data Rock = Round | Square

derive instance eqRock :: Eq Rock
derive instance ordRock :: Ord Rock
instance showRock :: Show Rock where
  show Round = "O"
  show Square = "#"

type Coord = Tuple Int Int

data Dir = N | S | E | W
data Axis = NS | EW
data Towards = Top | Bottom

solve1 :: String -> Int
solve1 = parse >>> tiltRocks N >>> totalLoad

solve2 :: String -> Int
solve2 = parse >>> findLoop >>> endState >>> totalLoad
  where
    findLoop = loop 1 Map.empty
      where
        loop n previous rocks =
          let spun = spinCycle rocks
          in case Map.lookup spun previous of
            Nothing -> loop (n + 1) (Map.insert spun n previous) spun
            Just startLoop ->
              { startLoop
              , period: n - startLoop
              , rocks: spun
              }

    endState { startLoop, period, rocks } =
      let n = (1000000000 - startLoop) `mod` period
      in pipeline (Array.replicate n spinCycle) rocks

    spinCycle = pipeline $ tiltRocks <$> [N, W, S, E]

tiltRocks :: Dir -> Map Coord Rock -> Map Coord Rock
tiltRocks dir rocks =
  fold { fallen: Map.empty, lastCoords: Map.empty } rocks # _.fallen
  where
    axis /\ towards = case dir of
      N -> NS /\ Top
      S -> NS /\ Bottom
      W -> EW /\ Top
      E -> EW /\ Bottom

    fallAxis :: Lens' Coord Int
    fallAxis = case axis of
      NS -> _2
      EW -> _1

    pivotAxis :: Lens' Coord Int
    pivotAxis = case axis of
      NS -> _1
      EW -> _2

    accum = case towards of
      Top -> (_ + 1)
      Bottom -> (_ - 1)

    bound = case dir of
      N -> 0
      W -> 0
      S -> findMaxY rocks
      E -> findMaxX rocks

    tiltr coord rock res = tiltl coord res rock
    tiltl coord { fallen, lastCoords } rock =
      let pivot = coord ^. pivotAxis
      in case rock of
        Square ->
          { fallen: Map.insert coord rock fallen
          , lastCoords: Map.insert pivot (coord ^. fallAxis) lastCoords
          }
        Round ->
          let
            fall = fromMaybe bound $ Map.lookup pivot lastCoords <#> accum
            fallenCoord = fallAxis .~ fall $ coord
          in
          { fallen: Map.insert fallenCoord rock fallen
          , lastCoords: Map.insert pivot fall lastCoords
          }

    fold = case towards of
      Top -> foldlWithIndex tiltl
      Bottom -> foldrWithIndex tiltr

totalLoad :: Map Coord Rock -> Int
totalLoad rocks =
  F.sum $ mapWithIndex (\(_ /\ y) -> case _ of
    Square -> 0
    Round -> rows - y
  ) rocks
  where
    rows = findMaxY rocks + 1

findMaxX :: forall a. Map Coord a -> Int
findMaxX = Map.keys >>> Set.map fst >>> Set.findMax >>> fromMaybe 0

findMaxY :: forall a. Map Coord a -> Int
findMaxY = Map.keys >>> Set.map snd >>> Set.findMax >>> fromMaybe 0

pipeline :: forall f a. Foldable f => f (a -> a) -> a -> a
pipeline fs a = F.foldl (#) a fs

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

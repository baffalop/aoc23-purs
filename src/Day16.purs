module Day16 where
-- The Floor Will Be Lava

import Prelude
import Utils.Geometry (parseGrid, rotateCcw, rotateCw)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple, uncurry)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Set as Set
import Utils.String (lines) as String
import Data.Array as Array
import Data.String.CodeUnits (length) as String
import Utils.Basics (mapBoth)
import Input (readInput)

data Optic
  = MirrorNE
  | MirrorNW
  | SplitterV
  | SplitterH

derive instance eqOptic :: Eq Optic
derive instance ordOptic :: Ord Optic
instance showOptic :: Show Optic where
  show = case _ of
    MirrorNE -> "/"
    MirrorNW -> "\\"
    SplitterV -> "|"
    SplitterH -> "-"

type Coord = Tuple Int Int

type Beam =
  { pos :: Coord
  , heading :: Coord
  }

solve1 :: String -> Int
solve1 s =
  Set.size $ project { pos: -1 /\ 0, heading: 1 /\ 0 } Set.empty
  where
    optics = parse s
    lines = String.lines s
    maxY = Array.length lines - 1
    maxX = maybe 0 (String.length >>> (_ - 1)) $ Array.head lines

    project { pos, heading } trail =
      let
        nextPos@(x /\ y) = pos + heading
        beam = { pos: nextPos, heading }
      in if not (between 0 maxX x) || not (between 0 maxY y) || Set.member beam trail
        then resolve trail
        else case Map.lookup nextPos optics of
          Nothing -> project beam $ Set.insert beam trail
          Just MirrorNE ->
            let nextBeam = beam { heading = reflectNE heading }
            in project nextBeam $ Set.insert nextBeam trail
          Just MirrorNW ->
            let nextBeam = beam { heading = reflectNW heading }
            in project nextBeam $ Set.insert nextBeam trail
          Just SplitterV ->
            if not $ isHoriz heading then project beam $ Set.insert beam trail
              else uncurry (<>)
                $ mapBoth (\h -> let nextBeam = beam { heading = h} in project nextBeam $ Set.insert nextBeam trail)
                $ (0 /\ 1) /\ (0 /\ -1)
          Just SplitterH ->
            if isHoriz heading then project beam $ Set.insert beam trail
              else uncurry (<>)
                $ mapBoth (\h -> let nextBeam = beam { heading = h} in project nextBeam $ Set.insert nextBeam trail)
                $ (1 /\ 0) /\ (-1 /\ 0)

    resolve = Set.map _.pos

reflectNE :: Coord -> Coord
reflectNE coord = (if isHoriz coord then rotateCcw else rotateCw) coord

reflectNW :: Coord -> Coord
reflectNW coord = (if isHoriz coord then rotateCw else rotateCcw) coord

isHoriz :: Coord -> Boolean
isHoriz (x /\ _) = x /= 0

parse :: String -> Map Coord Optic
parse = parseGrid $ case _ of
  '|' -> Just SplitterV
  '-' -> Just SplitterH
  '\\' -> Just MirrorNW
  '/' -> Just MirrorNE
  _ -> Nothing

example = """.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""

input = readInput 16

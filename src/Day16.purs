module Day16 where
-- The Floor Will Be Lava

import Prelude
import Utils.Geometry (parseGrid, rotateCcw, rotateCw)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Set (Set)
import Data.Set as Set
import Utils.String (lines) as String
import Data.Array as Array
import Data.String.CodeUnits (length) as String
import Input (readInput)
import Uncurried.State (State, execState)
import Data.Foldable (for_)
import Data.Lens.Record (prop) as Lens
import Type.Proxy (Proxy(Proxy))
import Data.Lens.Setter ((%=))
import Data.Lens.Getter (use) as Lens

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

type BeamState = State { beams :: Set Beam, energised :: Set Coord } Unit

solve1 :: String -> Int
solve1 s =
  Set.size $ _.energised
    $ execState { beams: Set.empty, energised: Set.empty }
    $ project { pos: -1 /\ 0, heading: 1 /\ 0 }
  where
    optics = parse s
    lines = String.lines s
    maxY = Array.length lines - 1
    maxX = maybe 0 (String.length >>> (_ - 1)) $ Array.head lines

    project :: Beam -> BeamState
    project { pos, heading } = do
      let
        nextPos@(x /\ y) = pos + heading
        beam = { pos: nextPos, heading }
      if not (between 0 maxX x) || not (between 0 maxY y)
      then pure unit
      else do
        _energised %= Set.insert nextPos
        case Map.lookup nextPos optics of
          Nothing -> project beam
          Just MirrorNE -> advance beam { heading = reflectNE heading }
          Just MirrorNW -> advance beam { heading = reflectNW heading }
          Just SplitterV ->
            if not (isHoriz heading) then advance beam
            else for_ [0 /\ 1, 0 /\ -1] \h -> advance beam { heading = h }
          Just SplitterH ->
            if isHoriz heading then advance beam
            else for_ [1 /\ 0, -1 /\ 0] \h -> advance beam { heading = h }

    advance :: Beam -> BeamState
    advance beam = do
      beams <- Lens.use _beams
      if Set.member beam beams then pure unit
        else do
          _beams %= Set.insert beam
          project beam

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

_beams = Lens.prop (Proxy :: Proxy "beams")
_energised = Lens.prop (Proxy :: Proxy "energised")

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

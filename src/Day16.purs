module Day16 where
-- The Floor Will Be Lava

import Prelude
import Utils.Geometry (parseGrid)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Map (Map)
import Data.Map as Map

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

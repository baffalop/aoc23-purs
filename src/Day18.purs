module Day18 where
-- Lavaduct Lagoon

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (linesOf)
import Parsing.String.Basic (intDecimal) as P
import Parsing.String (char, string) as P
import Utils.Parsing (wordAlphaNum) as P
import Parsing.Combinators (choice) as P
import Data.Tuple.Nested ((/\))
import Data.Tuple (Tuple(Tuple))
import Data.Either (Either)
import Utils.Pointfree ((<<#>>))
import Data.Foldable (class Foldable, foldr)
import Data.Traversable (class Traversable)
import Data.Array as Array
import Data.Set as Set
import Data.Set (Set)
import Data.Map as Map
import Data.Map (Map)
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromMaybe)
import Input (readInput)
import Data.Array ((..), (:))
import Utils.Basics (dup, mapBoth)
import Data.FoldableWithIndex (foldlWithIndex)

type Vec = Tuple Int Int

type Plan =
  { vec :: Vec
  , n :: Int
  , color :: String
  }

type Segment = Tuple Int Int

newtype DebugTrench = DebugTrench (Map Int (Set Int))

solve1 = parse <<#>> trenchArea

buildTrench :: forall f. Foldable f => f Plan -> { cols :: Map Int (Array Segment), rows :: Map Int (Array Segment) }
buildTrench = F.foldl
  (\{ coord: coord@(x1 /\ y1), trench } { vec, n } ->
    let newCoord@(x2 /\ y2) = coord + mapBoth (n * _) vec
    in
    { coord: newCoord
    , trench: if y1 == y2
      then trench { rows = Map.insertWith (<>) y1 [min x1 x2 /\ max x1 x2] trench.rows }
      else trench { cols = Map.insertWith (<>) x1 [min y1 y2 /\ max y1 y2] trench.cols }
    }
  )
  { coord: zero /\ zero, trench: { rows: Map.empty, cols: Map.empty }}
  >>> _.trench

trenchArea :: Array Plan -> Int
trenchArea plan =
  let
    trench@{ cols } = buildTrench plan
    { rows, area } = trench.rows
      # foldlWithIndex
        (\y { rows, area, lastY } newRows ->
          let
            combinedRows = difference newRows rows
            joints = edges combinedRows
              # Array.filter (\x -> fromMaybe false
                $ Array.any (\(from /\ to) -> from == y || to == y)
                <$> Map.lookup x cols
              )
              # map dup
          in
          { lastY: Just y
          , rows: union joints combinedRows
          , area: area + sumSegments (union newRows rows) + fromMaybe zero do
            lastY <- lastY
            pure $ sumSegments rows * (y - lastY - one)
          }
        )
        { rows: [], area: 0, lastY: Nothing }
  in area + sumSegments rows

difference :: Array Segment -> Array Segment -> Array Segment
difference = combineWith segmentDifference

union :: Array Segment -> Array Segment -> Array Segment
union = combineWith segmentUnion

edges :: Array Segment -> Array Int
edges segments = segments >>= \(from /\ to) -> [from - one, to + one]

combineWith :: (Segment -> Segment -> Maybe (Array Segment)) -> Array Segment -> Array Segment -> Array Segment
combineWith f newSegments existingSegments = F.foldr combineInto existingSegments newSegments
  where
    combineInto :: Segment -> Array Segment -> Array Segment
    combineInto newSegment segments =
      let
        { res, comp} = F.foldr
          (\s acc@{ res, comp } ->
            case Array.findMap (\c -> Tuple c <$> f c s) comp of
              Nothing -> acc { res = s : res }
              Just (c /\ diff) -> acc { comp = diff <> Array.filter (_ /= c) comp }
          )
          { res: [], comp: [newSegment] }
          segments
    in res <> comp

segmentDifference :: Segment -> Segment -> Maybe (Array Segment)
segmentDifference s1 s2 =
  if to1 < from2 then Nothing
  else Just $ Array.mapMaybe validate
    [ from1 /\ min (from2 - one) to1
    , max from2 (to1 + one) /\ to2
    , (to2 + 1) /\ to1
    ]
  where
    validate s@(from /\ to) = if from > to then Nothing else Just s
    (from1 /\ to1) = min s1 s2
    (from2 /\ to2) = max s1 s2

segmentUnion :: Segment -> Segment -> Maybe (Array Segment)
segmentUnion s1 s2 =
  if to1 < from2 then Nothing else Just [from1 /\ (max to1 to2)]
  where
    (from1 /\ to1) = min s1 s2
    (from2 /\ to2) = max s1 s2

sumSegments :: Array Segment -> Int
sumSegments = F.sum <<< map (\(from /\ to) -> to - from + 1)

zero = 0
one = 1

parse :: String -> Either ParseError (Array Plan)
parse s = runParser s $ linesOf do
  vec <- vector <* P.char ' '
  n <- P.intDecimal <* P.char ' '
  color <- P.string "(#" *> P.wordAlphaNum <* P.char ')'
  pure { vec, n, color }
  where
    vector = P.choice
      [ 0 /\ 1 <$ P.char 'D'
      , 0 /\ -1 <$ P.char 'U'
      , 1 /\ 0 <$ P.char 'R'
      , -1 /\ 0 <$ P.char 'L'
      ]

instance showTrench :: Show DebugTrench where
  show (DebugTrench trench) =
    trench <#> (\coords ->
      F.fold $ (\x -> if x `Set.member` coords then "#" else ".") <$> (xmin .. xmax)
    )
    # F.intercalate "\n"
    where
      allXs = F.fold trench
      xmin = fromMaybe 0 $ Set.findMin allXs
      xmax = fromMaybe 0 $ Set.findMax allXs

example = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""

input = readInput 18

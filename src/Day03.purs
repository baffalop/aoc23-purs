module Day03 where
-- Gear Ratios

import Prelude
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Data.Foldable as F
import Utils.String (lines)
import Data.Foldable (fold)
import Data.Map as Map
import Parsing (Position(Position), runParser)
import Parsing.Combinators.Array (many) as P
import Data.Either (fromRight)
import Parsing.String (anyTill, match, satisfy) as P
import Parsing.String.Basic (intDecimal) as P
import Data.Array (filter, mapWithIndex, (..), (:))
import Parsing (position) as P
import Data.String as String
import Data.CodePoint.Unicode (isNumber) as C
import Data.String.CodePoints (codePointFromChar)
import Data.Maybe (Maybe(Just), isJust)
import Data.Array (length) as Array
import Input (readInput)

newtype Schematic = Scheme
  { parts :: Array Part
  , widgets :: Map Coord Char
  }

type Part =
  { n :: Int
  , length :: Int
  , coord :: Coord
  }

type Coord = { row :: Int, col :: Int }

derive newtype instance schemeShow :: Show Schematic

instance Semigroup Schematic where
  append (Scheme { parts: p1, widgets: w1 }) (Scheme { parts: p2, widgets: w2 }) =
    Scheme
      { parts: p1 <> p2
      , widgets: Map.union w1 w2
      }

instance Monoid Schematic where
  mempty = Scheme
    { parts: []
    , widgets: Map.empty
    }

solve1 :: String -> Int
solve1 s =
  let Scheme { parts, widgets } = parse s
  in parts
    # filter (neighbours >>> F.any (flip Map.lookup widgets >>> isJust))
    # map _.n
    # F.sum

solve2 :: String -> Int
solve2 s =
  let
    Scheme { parts, widgets } = parse s
    cogs = [] <$ Map.filter (_ == '*') widgets
    allNeighbours = do
      part@{ n } <- parts
      { n, coord: _ } <$> neighbours part
  in
  F.foldr
    (\{ n, coord } -> Map.update (Just <<< (n : _)) coord)
    cogs
    allNeighbours
    # Map.filter (Array.length >>> (_ == 2))
    <#> F.product
    # F.sum

neighbours :: Part -> Array Coord
neighbours { length, coord: { row, col } } =
  [ { row, col: col - 1 }
  , { row, col: col + length }
  ]
  <> map { row: row - 1, col: _ } cols
  <> map { row: row + 1, col: _ } cols
  where
    cols = (col - 1) .. (col + length)

parse :: String -> Schematic
parse = fold <<< mapWithIndex parseLine <<< lines
  where
    parseLine :: Int -> String -> Schematic
    parseLine row l =
      let
        parts = runParser l $ P.many $ part row
        widgets = runParser l $ P.many $ widget row
      in Scheme
        { parts: fromRight [] parts
        , widgets: Map.fromFoldable $ fromRight [] widgets
        }

    part row = do
      Tuple _ (Tuple nStr n) <- P.anyTill $ P.match P.intDecimal
      Position { index } <- P.position
      let length = String.length nStr
      pure
        { n
        , length
        , coord: { row, col: index - length }
        }

    widget row = do
      Tuple _ w <- P.anyTill $ P.satisfy \c -> c /= '.' && not (C.isNumber $ codePointFromChar c)
      Position { index } <- P.position
      pure $ Tuple { row, col: index - 1 } w

example = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

input = readInput 3

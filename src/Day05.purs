module Day05 where

import Prelude
import Input (readInput)
import Parsing (ParseError, parseErrorMessage, runParser)
import Parsing.String (char, string) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.Combinators (sepBy, sepBy1, sepEndBy) as P
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NL
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..))
import Utils.Parsing (word) as P
import Data.Tuple (Tuple(Tuple))
import Data.Traversable (traverse)
import Data.Either (either, note) as Either
import Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable (minimum) as NF

type Almanac =
  { seeds :: NonEmptyList Int
  , mappings :: Map String Mapping
  }

type Mapping =
  { kind :: String
  , ranges :: List Range
  }

type Range =
  { from :: Int
  , to :: Int
  , offset :: Int
  }

solve1 s = do
  { seeds, mappings } <- mapLeft parseErrorMessage $ parse s
  let
    translate { kind, id } = do
      { kind, ranges } <- Either.note ("No mapping from: " <> kind) $ Map.lookup kind mappings
      let
        offset = fromMaybe 0 $ _.offset <$> F.find (\{ from, to } -> id >= from && id <= to) ranges
        translated = id + offset
      if kind == "location" then pure translated else translate { kind, id: translated }

  NF.minimum <$> traverse (translate <<< { kind: "seed", id: _ }) seeds

parse :: String -> Either ParseError Almanac
parse = flip runParser do
  seeds <- P.string "seeds: " *> P.intDecimal `P.sepBy1` P.char ' '
  _ <- P.string "\n\n"
  mappings <- Map.fromFoldable <$> mapping `P.sepBy` P.char '\n'
  pure { seeds, mappings }
  where
    mapping = do
      source <- P.word
      destination <- P.string "-to-" *> P.word
      _ <- P.string " map:\n"
      ranges <- range `P.sepEndBy` P.char '\n'
      pure $ Tuple source { kind: destination, ranges }

    range = do
      target <- P.intDecimal <* P.char ' '
      from <- P.intDecimal <* P.char ' '
      length <- P.intDecimal
      pure { from, to: from + length - 1, offset: target - from }

mapLeft :: forall a b c. (a -> b) -> Either a c -> Either b c
mapLeft f = Either.either (Left <<< f) (Right <<< identity)

example = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

input = readInput 5

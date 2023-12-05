module Day05 where

import Prelude
import Input (readInput)
import Parsing (ParseError, parseErrorMessage, runParser)
import Parsing.String (char, string) as P
import Parsing.Combinators (sepBy, sepBy1, sepEndBy) as P
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NL
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..))
import Utils.Parsing (bigInt, word) as P
import Data.Tuple (Tuple(Tuple))
import Data.Traversable (traverse)
import Data.Either (either, note) as Either
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup.Foldable (minimum) as NF
import Data.BigInt (BigInt)
import Data.BigInt as BigInt

type Almanac =
  { seeds :: NonEmptyList BigInt
  , mappings :: Map String Mapping
  }

type Mapping =
  { kind :: String
  , translations :: List Translation
  }

type Translation =
  { range :: Range
  , offset :: BigInt
  }

type Range =
  { from :: BigInt
  , to :: BigInt
  }

solve1 :: String -> Either String BigInt
solve1 s = do
  { seeds, mappings } <- mapLeft parseErrorMessage $ parse s
  NF.minimum <$> traverse (translateWith mappings translate) seeds
  where
    translate translations id =
      F.find (\{ range: { from, to } } -> id >= from && id <= to) translations
        <#> _.offset
        # fromMaybe (BigInt.fromInt 0)
        # (id + _)

solve2 :: String -> Either String BigInt
solve2 s = do
  { seeds, mappings } <- mapLeft parseErrorMessage $ parse s
  translated <- translateWith mappings translate $ toRanges $ NL.toList seeds
  Either.note "no seeds"
    $ F.minimum $ _.from <$> translated
  where
    toRanges (start : length : rest) = rangeFrom start length : toRanges rest
    toRanges _ = Nil

    translate :: List Translation -> List Range -> List Range
    translate translations ranges = ranges >>= translateBy translations

    translateBy :: List Translation -> Range -> List Range
    translateBy Nil range = range : Nil
    translateBy ({ range: transRange, offset } : translations) range =
        let { within, remainder } = intersect range transRange
        in (bump offset <$> within) ?: translate translations remainder

    bump offset { from, to } = { from: from + offset, to: to + offset }

translateWith :: forall a. Map String Mapping -> (List Translation -> a -> a) -> a -> Either String a
translateWith mappings translate = doTranslate "seed"
  where
    doTranslate kind subject = do
      { kind: toKind, translations } <- Either.note ("No mapping from: " <> kind) $ Map.lookup kind mappings
      let translated = translate translations subject
      if toKind == "location" then pure translated else doTranslate toKind translated

parse :: String -> Either ParseError Almanac
parse = flip runParser do
  seeds <- P.string "seeds: " *> P.bigInt `P.sepBy1` P.char ' '
  _ <- P.string "\n\n"
  mappings <- Map.fromFoldable <$> mapping `P.sepBy` P.char '\n'
  pure { seeds, mappings }
  where
    mapping = do
      source <- P.word
      destination <- P.string "-to-" *> P.word
      _ <- P.string " map:\n"
      translations <- translation `P.sepEndBy` P.char '\n'
      pure $ Tuple source { kind: destination, translations }

    translation = do
      target <- P.bigInt <* P.char ' '
      start <- P.bigInt <* P.char ' '
      length <- P.bigInt
      pure
        { range: rangeFrom start length
        , offset: target - start
        }

intersect :: Range -> Range -> { within :: Maybe Range, remainder :: List Range }
intersect range target =
  { within: validate
    { from: max range.from target.from
    , to: min range.to target.to
    }
  , remainder:
    validate range { to = min range.to $ target.from - BigInt.fromInt 1 }
    ?: validate range { from = max range.from $ target.to + BigInt.fromInt 1 }
    ?: Nil
  }
  where
    validate range'@{ from, to } = if to < from then Nothing else Just range'

rangeFrom :: BigInt -> BigInt -> Range
rangeFrom start length =
  { from: start
  , to: start + length - BigInt.fromInt 1
  }

maybeCons :: forall a. Maybe a -> List a -> List a
maybeCons Nothing = identity
maybeCons (Just x) = (x : _)

infixr 6 maybeCons as ?:

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

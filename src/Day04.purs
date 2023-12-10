module Day04 where

import Prelude
import Parsing (ParseError, runParser)
import Data.Set (Set)
import Data.Either (Either)
import Parsing.String (string) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.Combinators (sepEndBy) as P
import Utils.Parsing (linesOf, spaces) as P
import Data.Set (fromFoldable, intersection, size) as Set
import Utils.Pointfree ((<<$>>))
import Data.Int (pow)
import Data.Foldable as F
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((..))
import Data.Array (length) as Array
import Input (readInput)

type Card =
  { id :: Int
  , winning :: Set Int
  , have :: Set Int
  }

solve1 :: String -> Either ParseError Int
solve1 =
  F.sum <<< map (score <<< winnings) <<$>> parse
  where
    score 0 = 0
    score n = 2 `pow` (n - 1)

solve2 :: String -> Either ParseError Int
solve2 s = do
  cards <- parse s
  cards
    # F.foldl
        (\copiesBefore card@{ id } ->
          let
            copies = Map.insertWith (+) id 1 copiesBefore
            count = fromMaybe 1 $ Map.lookup id copies
            won = winnings card
            toCopy = if won == 0 then [] else (id + 1) .. (id + won)
          in
          F.foldr
            (\copyId -> Map.insertWith (+) copyId count)
            copies
            toCopy
        )
        Map.empty
    # Map.submap Nothing (Just $ Array.length cards)
    # F.sum
    # pure

winnings :: Card -> Int
winnings { winning, have } = Set.size $ Set.intersection winning have

parse :: String -> Either ParseError (Array Card)
parse s = runParser s $ P.linesOf do
  id <- P.string "Card" *> P.spaces *> P.intDecimal <* P.string ":"
  winning <- numberSet
  _ <- P.string "|"
  have <- numberSet
  pure { id, winning, have }
  where
    numberSet = P.spaces *> (Set.fromFoldable <$> P.intDecimal `P.sepEndBy` P.spaces)

example = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

input = readInput 4

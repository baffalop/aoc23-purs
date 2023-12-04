module Day04 where

import Prelude
import Utils.Parsing (linesOf)
import Parsing (ParseError, runParser)
import Data.Set (Set)
import Data.Either (Either)
import Data.List (List)
import Parsing.String (char, eof, string) as P
import Parsing.String.Basic (intDecimal, space) as P
import Parsing.Combinators (many, many1Till, skipMany1) as P
import Data.Set (fromFoldable) as Set
import Data.Foldable (class Foldable)
import Control.Alt ((<|>))

type Card =
  { id :: Int
  , winning :: Set Int
  , have :: Set Int
  }

parse :: String -> Either ParseError (List Card)
parse s = runParser s $ P.many do
    id <- P.string "Card " *> P.intDecimal <* P.string ":"
    winning <- numberSet $ unit <$ P.string " |"
    have <- numberSet $ unit <$ P.char '\n' <|> P.eof
    pure { id, winning, have }
  where
    numberSet end =
      Set.fromFoldable <$> P.many1Till (P.skipMany1 P.space *> P.intDecimal) end

example = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

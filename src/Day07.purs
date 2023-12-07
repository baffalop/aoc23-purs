module Day07 where

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (intDigit, linesOf) as P
import Parsing.String (char) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.Combinators (choice, many1) as P
import Data.Either (Either)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NL

data Card
  = N Int
  | T
  | J
  | Q
  | K
  | A

derive instance eqCard :: Eq Card
derive instance ordCard :: Ord Card
instance showCard :: Show Card where
    show (N n) = show n
    show T = "T"
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"

type Play =
  { hand :: NonEmptyList Card
  , bid :: Int
  }

parse :: String -> Either ParseError (List Play)
parse s = runParser s $ P.linesOf $ { hand: _, bid: _ } <$> hand <* P.char ' ' <*> P.intDecimal
  where
    hand = P.many1 $ P.choice
      [ N <$> P.intDigit
      , T <$ P.char 'T'
      , J <$ P.char 'J'
      , Q <$ P.char 'Q'
      , K <$ P.char 'K'
      , A <$ P.char 'A'
      ]

example = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

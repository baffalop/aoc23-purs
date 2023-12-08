module Day07 where

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (intDigit, linesOf) as P
import Parsing.String (char) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.Combinators (choice) as P
import Parsing.Combinators.Array (many) as P
import Data.Either (Either)
import Utils.Pointfree ((<<#>>))
import Data.Array as Array
import Data.Tuple (Tuple(Tuple))
import Data.Foldable (sum) as F
import Input (readInput)
import Utils.Map as UM
import Data.List (List(..), (:))
import Data.List as List
import Utils.List (sortDesc) as UL

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
  show = case _ of
    (N n) -> show n
    T -> "T"
    J -> "J"
    Q -> "Q"
    K -> "K"
    A -> "A"

data Score
  = High
  | Pair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind

derive instance eqScore :: Eq Score
derive instance ordScore :: Ord Score

instance showScore :: Show Score where
  show = case _ of
    High -> "High"
    Pair -> "Pair"
    TwoPair -> "TwoPair"
    ThreeKind -> "ThreeKind"
    FullHouse -> "FullHouse"
    FourKind -> "FourKind"
    FiveKind -> "FiveKind"

newtype WithJoker = WithJoker Card
derive newtype instance eqJoke :: Eq WithJoker

instance ordJoke :: Ord WithJoker where
  compare (WithJoker J) (WithJoker J) = EQ
  compare (WithJoker J) _ = LT
  compare _ (WithJoker J) = GT
  compare (WithJoker c1) (WithJoker c2) = compare c1 c2

instance showJoke :: Show WithJoker where
  show (WithJoker card) = show card

type Play =
  { hand :: Array Card
  , bid :: Int
  }

solve1 :: String -> Either ParseError Int
solve1 = parse
  <<#>> Array.sortWith (\{ hand } -> Tuple (score hand) hand)
  >>> Array.mapWithIndex (\rank { bid } -> (rank + 1) * bid)
  >>> F.sum

solve2 :: String -> Either ParseError Int
solve2 = parse
  <<#>> Array.sortWith (\{ hand } -> Tuple (scoreWithJoker hand) $ WithJoker <$> hand)
  >>> Array.mapWithIndex (\rank { bid } -> (rank + 1) * bid)
  >>> F.sum
  where
    scoreWithJoker :: Array Card -> Score
    scoreWithJoker hand =
      let { yes: jokers, no: rest } = Array.partition (_ == J) hand
      in case sortedCounts rest of
        Nil -> FiveKind
        (most : others) -> scoreSortedCounts $ (most + Array.length jokers) : others

score :: Array Card -> Score
score = sortedCounts >>> scoreSortedCounts

sortedCounts :: Array Card -> List Int
sortedCounts = UM.counts >>> List.fromFoldable >>> UL.sortDesc

scoreSortedCounts :: List Int -> Score
scoreSortedCounts = case _ of
  5 : _ -> FiveKind
  4 : _ -> FourKind
  3 : 2 : _ -> FullHouse
  3 : _ -> ThreeKind
  2 : 2 : _ -> TwoPair
  2 : _ -> Pair
  _ -> High

parse :: String -> Either ParseError (Array Play)
parse s = runParser s $ P.linesOf $ { hand: _, bid: _ } <$> hand <* P.char ' ' <*> P.intDecimal
  where
    hand = P.many $ P.choice
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

input = readInput 7

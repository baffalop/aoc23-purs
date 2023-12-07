module Day07 where

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (intDigit, linesOf) as P
import Parsing.String (char) as P
import Parsing.String.Basic (intDecimal) as P
import Parsing.Combinators (choice) as P
import Parsing.Combinators.Array (many) as P
import Data.Either (Either)
import Utils.Pointfree ((<<#>>), (<<$>>))
import Data.Array as Array
import Data.Tuple (Tuple(Tuple))
import Data.Map as Map
import Data.Tuple as Tuple
import Data.Foldable (sum) as F
import Input (readInput)
import Data.Maybe (fromMaybe)
import Utils.Map as UM

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
    scoreWithJoker hand =
      let
        { yes: jokers, no: rest } = Array.partition (_ == J) hand
        mostFrequent = fromMaybe J $ _.value <<$>> Map.findMax $ UM.invert $ UM.counts rest
      in score $ (mostFrequent <$ jokers) <> rest

score :: Array Card -> Score
score hand = case Array.sort $ Array.fromFoldable $ UM.counts hand of
  [5] -> FiveKind
  [1, 4] -> FourKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeKind
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> Pair
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

module Day19 where
-- Aplenty

import Prelude
import Parsing (ParseError, parseErrorMessage, runParser)
import Data.Map as Map
import Data.Map (Map)
import Parsing.String (char) as P
import Parsing.Combinators (choice, many, tryRethrow) as P
import Parsing.Combinators.Array (many) as PA
import Utils.Parsing (arraySepBy, word) as P
import Data.Tuple (Tuple(Tuple))
import Data.List as List
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Parsing.String.Basic (intDecimal) as P
import Data.Lens.Record (prop) as Lens
import Type.Proxy (Proxy(Proxy))
import Data.Lens.Traversal (traversed)
import Data.Lens.Setter ((%~), (.~))
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Array as Array
import Data.Foldable as F
import Input (readInput)
import Control.Alt ((<|>))
import Data.Lens.Types (Lens')
import Data.Lens.Getter ((^.))
import Data.Tuple.Nested ((/\))
import Data.BigInt as BigInt
import Data.BigInt (BigInt)

type Process =
  { workflows :: Map Label Workflow
  , parts :: Array Part
  }

type Workflow =
  { rules :: List Rule
  , fallback :: Result
  }

type Rule =
  { stat :: Stat
  , comparison :: Comparison
  , value :: Int
  , result :: Result
  }

type Stats a =
  { x :: a
  , m :: a
  , a :: a
  , s :: a
  }

type Part = Stats Int

data Comparison = GT | LT
instance showComparison :: Show Comparison where
  show GT = ">"
  show LT = "<"

data Stat = X | M | A | S
instance showStat :: Show Stat where
  show = case _ of
    X -> "x"
    M -> "m"
    A -> "a"
    S -> "s"

newtype Label = Label String
derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label
derive newtype instance showLabel :: Show Label

data Result
  = Accept
  | Reject
  | Next Label

derive instance eqResult :: Eq Result
instance showResult :: Show Result where
  show Accept = "A"
  show Reject = "R"
  show (Next label) = show label

type Range = Tuple Int Int
type Ranges = Stats Range

solve1 :: String -> Either String Int
solve1 s = do
  { workflows, parts } <- lmap parseErrorMessage $ parse s
  let
    process label part = case Map.lookup label workflows of
      Nothing -> Left $ "Label not found: " <> show label
      Just workflow -> case processThrough workflow part of
        Next nextLabel -> process nextLabel part
        result -> pure { part, result }

  results <- traverse (process $ Label "in") parts
  results
    # Array.mapMaybe (\{ part, result } -> if result == Accept then Just $ score part else Nothing)
    # F.sum
    # pure

processThrough :: Workflow -> Part -> Result
processThrough { rules, fallback } part = apply rules
  where
    apply Nil = fallback
    apply ({ stat, comparison, value, result } : rest) =
      let
        test = case comparison of
          GT -> (>)
          LT -> (<)
      in if (part ^. statLens stat) `test` value then result else apply rest

solve2 :: String -> Either String BigInt
solve2 s = do
  { workflows } <- lmap parseErrorMessage $ parse s
  let
    acceptedCombinations label ranges = case Map.lookup label workflows of
      Nothing -> Left $ "Label not found: " <> show label
      Just workflow -> forWorkflow workflow ranges

    forWorkflow { rules, fallback } = forRules rules
      where
        forRules Nil ranges = forResult fallback ranges
        forRules ({ stat, comparison, value, result } : rest) ranges =
          let
            _stat = statLens stat
            { yes, no } = partitionBy comparison value $ ranges ^. _stat
            noResult = no # maybe (pure []) \r -> forRules rest $ (_stat .~ r) ranges
            yesResult = yes # maybe (pure []) \r -> forResult result $ (_stat .~ r) ranges
          in (<>) <$> yesResult <*> noResult

    forResult result ranges = case result of
      Reject -> pure []
      Accept -> pure [ranges]
      Next label -> acceptedCombinations label ranges

  combinations <- acceptedCombinations (Label "in") baseRanges
  pure $ F.sum $ forRanges <$> combinations
  where
    baseRanges = { x: baseRange, m: baseRange, a: baseRange, s: baseRange }
    baseRange = 1 /\ 4000
    forRanges { x, m, a, s } = total x * total m * total a * total s
    total (lo /\ hi) = BigInt.fromInt $ hi - lo + 1

partitionBy :: Comparison -> Int -> Range -> { yes :: Maybe Range, no :: Maybe Range }
partitionBy comparison value (lo /\ hi) = case comparison of
  LT ->
    { yes: if value <= lo then Nothing else Just $ lo /\ min hi (value - 1)
    , no: if value > hi then Nothing else Just $ max lo value /\ hi
    }
  GT ->
    { yes: if value >= hi then Nothing else Just $ max lo (value + 1) /\ hi
    , no: if value < lo then Nothing else Just $ lo /\ min hi value
    }

statLens :: forall a. Stat -> Lens' (Stats a) a
statLens = case _ of
  X -> Lens.prop (Proxy :: Proxy "x")
  M -> Lens.prop (Proxy :: Proxy "m")
  A -> Lens.prop (Proxy :: Proxy "a")
  S -> Lens.prop (Proxy :: Proxy "s")

score :: Part -> Int
score { x, m, a, s } = x + m + a + s

parse :: String -> Either ParseError Process
parse s = runParser s $ { workflows: _, parts: _ } <$> workflows <* P.char '\n' <*> parts
  where
    workflows = Map.fromFoldable <$> PA.many do
      label <- Label <$> P.word <* P.char '{'
      rules <- P.many $ P.tryRethrow rule <* P.char ','
      fallback <- result <* P.char '}'
      _ <- P.char '\n'
      pure $ Tuple label { rules, fallback }

    rule = do
      stat <- P.choice
        [ X <$ P.char 'x'
        , M <$ P.char 'm'
        , A <$ P.char 'a'
        , S <$ P.char 's'
        ]
      comparison <- LT <$ P.char '<' <|> GT <$ P.char '>'
      value <- P.intDecimal
      result <- P.char ':' *> result
      pure { stat, comparison, value, result }

    result = P.choice
      [ Accept <$ P.char 'A'
      , Reject <$ P.char 'R'
      , Next <<< Label <$> P.word
      ]

    parts = flip P.arraySepBy (P.char '\n') do
      x <- P.char '{' *> stat 'x'
      m <- P.char ',' *> stat 'm'
      a <- P.char ',' *> stat 'a'
      s <- P.char ',' *> stat 's' <* P.char '}'
      pure { x, m, a, s }

    stat s = P.char s *> P.char '=' *> P.intDecimal

showProcess :: Process -> String
showProcess = show <<< (_workflows <<< traversed <<< _rules <<< traversed %~ _.result)

_workflows = Lens.prop (Proxy :: Proxy "workflows")
_rules = Lens.prop (Proxy :: Proxy "rules")

example = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"""

input = readInput 19

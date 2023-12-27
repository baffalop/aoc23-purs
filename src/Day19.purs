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
import Data.Lens.Setter ((%~))
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Array (mapMaybe) as Array
import Data.Foldable (sum) as F
import Input (readInput)

type Process =
  { workflows :: Map Label Workflow
  , parts :: Array Part
  }

type Workflow =
  { rules :: List Rule
  , fallback :: Result
  }

type Rule =
  { condition :: Part -> Boolean
  , result :: Result
  }

type Part =
  { x :: Int
  , m :: Int
  , a :: Int
  , s :: Int
  }

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
    apply ({ condition, result } : rest) = if condition part then result else apply rest

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
      category <- P.choice
        [ _.x <$ P.char 'x'
        , _.m <$ P.char 'm'
        , _.a <$ P.char 'a'
        , _.s <$ P.char 's'
        ]
      comparison <- P.choice
        [ (<) <$ P.char '<'
        , (>) <$ P.char '>'
        ]
      value <- P.intDecimal
      result <- P.char ':' *> result
      pure
        { condition: category >>> (_ `comparison` value)
        , result
        }

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

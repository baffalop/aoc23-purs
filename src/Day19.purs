module Day19 where
-- Aplenty

import Prelude
import Parsing (ParseError, runParser)
import Data.Map as Map
import Data.Map (Map)
import Parsing.String (char) as P
import Parsing.Combinators (choice, many) as P
import Parsing.Combinators.Array (many) as PA
import Utils.Parsing (arraySepBy, word) as P
import Data.Tuple (Tuple(Tuple))
import Data.List as List
import Data.List (List)
import Data.Either (Either)
import Parsing.String.Basic (intDecimal) as P
import Data.Lens.Record (prop) as Lens
import Type.Proxy (Proxy(Proxy))
import Data.Lens.Traversal (traversed)
import Data.Lens.Setter ((%~))

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

instance showResult :: Show Result where
  show Accept = "A"
  show Reject = "R"
  show (Next label) = show label

parse :: String -> Either ParseError Process
parse s = runParser s $ { workflows: _, parts: _ } <$> workflows <* P.char '\n' <*> parts
  where
    workflows = Map.fromFoldable <$> PA.many do
      label <- Label <$> P.word <* P.char '{'
      rules <- P.many $ rule <* P.char ','
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

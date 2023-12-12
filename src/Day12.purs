module Day12 where

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (linesOf)
import Parsing.Combinators.Array (many) as P
import Parsing.Combinators (choice) as P
import Parsing.String (char) as P
import Utils.Parsing (arraySepBy) as P
import Parsing.String.Basic (intDecimal) as P
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Lens.Record (prop) as L
import Type.Proxy (Proxy(Proxy))
import Data.Lens.Setter ((%~))
import Utils.Pointfree ((<<$>>))
import Utils.Array ((<:), (<?:))

data Spring = Op | Damaged | Unknown
derive instance eqSpring :: Eq Spring

type Conditions = { springs :: Array Spring, contiguous :: Array Int }

instance showSpring :: Show Spring where
  show = case _ of
    Op -> "."
    Damaged -> "#"
    Unknown -> "?"

solve1 = map (_springs %~ toContiguous) <<$>> parse

toContiguous :: Array Spring -> Array (Tuple Spring Int)
toContiguous = Array.foldl f { cur: Nothing, res: [] } >>> \{ cur, res } -> res <?: cur
  where
    f whole@{ cur, res } spring = case cur /\ spring of
      _ /\ Op -> { cur: Nothing, res: res <?: cur }
      Nothing /\ _ -> { cur: Just $ spring /\ 1, res }
      Just current@(focus /\ count) /\ _
        | focus == spring -> whole { cur = Just $ focus /\ (count + 1) }
        | otherwise -> { cur: Just $ spring /\ 1, res: res <: current }

parse :: String -> Either ParseError (Array Conditions)
parse s = runParser s $ linesOf do
  springs <- P.many $ P.choice
    [ Op <$ P.char '.'
    , Damaged <$ P.char '#'
    , Unknown <$ P.char '?'
    ]
  _ <- P.char ' '
  contiguous <- P.intDecimal `P.arraySepBy` P.char ','
  pure { springs, contiguous }

_springs = L.prop (Proxy :: Proxy "springs")

example = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

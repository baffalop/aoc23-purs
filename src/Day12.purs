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

data Spring = Op | Damaged | Unknown
derive instance eqSpring :: Eq Spring

type Conditions = { springs :: Array Spring, contiguous :: Array Int }

instance showSpring :: Show Spring where
  show = case _ of
    Op -> "."
    Damaged -> "#"
    Unknown -> "?"

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

example = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

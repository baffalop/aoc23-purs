module Day08 where

import Prelude
import Input (readInput)
import Parsing (ParseError, Parser, runParser)
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Parsing.Combinators.Array (many) as P
import Parsing.String (char, string) as P
import Control.Alt ((<|>))
import Utils.Parsing (linesOf, word) as P
import Data.Either (Either)

data Step = L | R

type Navigation =
  { steps :: Array Step
  , network :: Network
  }

type Network = Map String (Tuple String String)

instance showStep :: Show Step where
  show L = "L"
  show R = "R"

parse :: String -> Either ParseError Navigation
parse s = runParser s do
  steps <- P.many $ L <$ P.char 'L' <|> R <$ P.char 'R'
  _ <- P.string "\n\n"
  network <- networkParser
  pure { steps, network }
  where
    networkParser :: Parser String Network
    networkParser = Map.fromFoldable <$> P.linesOf do
      from <- P.word <* P.string " = ("
      left <- P.word <* P.string ", "
      right <- P.word <* P.char ')'
      pure $ Tuple from $ Tuple left right

example = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

input = readInput 8

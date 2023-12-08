module Day08 where

import Prelude
import Input (readInput)
import Parsing (ParseError, Parser, parseErrorMessage, runParser)
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Parsing.Combinators (many) as P
import Parsing.String (char, string) as P
import Control.Alt ((<|>))
import Utils.Parsing (linesOf, word) as P
import Data.Either (Either)
import Data.List as List
import Data.List (List(..), (:))
import Data.Either (note) as Either
import Data.Bifunctor (lmap)

data Step = L | R

type Navigation =
  { steps :: List Step
  , network :: Network
  }

type Network = Map String (Tuple String String)

instance showStep :: Show Step where
  show L = "L"
  show R = "R"

--solve1 :: String -> Either ParseError
solve1 s = do
  { steps, network } <- lmap parseErrorMessage $ parse s
  let
    navigate :: String -> List Step -> Int -> Either String Int
    navigate "ZZZ" _ n = pure n
    navigate loc Nil n = navigate loc steps n
    navigate loc (step : restSteps) n = do
      Tuple left right <- Either.note ("location not found: " <> loc) $ Map.lookup loc network
      let nextLoc = case step of
            L -> left
            R -> right
      navigate nextLoc restSteps (n + 1)

  navigate "AAA" steps 0

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

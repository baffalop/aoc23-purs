module Day08 where

import Prelude
import Input (readInput)
import Parsing (ParseError, Parser, parseErrorMessage, runParser)
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Parsing.Combinators (many) as P
import Parsing.String (char, string) as P
import Control.Alt ((<|>))
import Utils.Parsing (linesOf, wordAlphaNum) as P
import Data.Either (Either(..))
import Data.List as List
import Data.List (List(..), (:))
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable as F
import Data.String (drop) as String
import Utils.Pointfree ((<<$>>))
import Data.Set as Set
import Data.Set (Set)
import Utils.Basics (mapBoth)

data Step = L | R

type Navigation =
  { steps :: List Step
  , network :: Network
  }

type Network = Map String (Tuple String String)

instance showStep :: Show Step where
  show L = "L"
  show R = "R"

solve1 :: String -> Either String Int
solve1 s = do
  { steps, network } <- lmap parseErrorMessage $ parse s
  let
    navigate :: String -> List Step -> Int -> Either String Int
    navigate "ZZZ" _ n = pure n
    navigate loc Nil n = navigate loc steps n
    navigate loc (step : restSteps) n =
      case follow step <$> Map.lookup loc network of
        Nothing -> Left $ "location not found: " <> loc
        Just nextLoc -> navigate nextLoc restSteps (n + 1)

  navigate "AAA" steps 0

solve2 :: String -> Either String Int
solve2 s = do
  { steps, network: naiveNetwork } <- lmap parseErrorMessage $ parse s
  let network = remapNetwork naiveNetwork
  let
    navigate :: List Step -> Int -> Set String -> Either String Int
    navigate Nil n locs = navigate steps n locs
    navigate (step : restSteps) n locs
      | F.all (endsIn "Z") locs = pure n
      | otherwise = navigate restSteps (n + 1)
        $ Set.mapMaybe (follow step <<$>> flip Map.lookup network) locs

  navigate steps 0 $ Set.filter (endsIn "A") $ Map.keys network
  where
    endsIn c = String.drop 2 >>> (_ == c)

remapNetwork :: Network -> Network
remapNetwork network =
  let
    shortcuts = network # Map.mapMaybe \(Tuple left right) -> if left == right then Just left else Nothing
  in if Map.isEmpty shortcuts then network
  else let
    remapped = network # Map.mapMaybeWithKey \k branch ->
      if Map.member k shortcuts then Nothing
      else Just $ mapBoth (\dest -> Map.lookup dest shortcuts # fromMaybe dest) branch
  in remapNetwork remapped

follow :: forall a. Step -> Tuple a a -> a
follow L = Tuple.fst
follow R = Tuple.snd

parse :: String -> Either ParseError Navigation
parse s = runParser s do
  steps <- P.many $ L <$ P.char 'L' <|> R <$ P.char 'R'
  _ <- P.string "\n\n"
  network <- networkParser
  pure { steps, network }
  where
    networkParser :: Parser String Network
    networkParser = Map.fromFoldable <$> P.linesOf do
      from <- P.wordAlphaNum <* P.string " = ("
      left <- P.wordAlphaNum <* P.string ", "
      right <- P.wordAlphaNum <* P.char ')'
      pure $ Tuple from $ Tuple left right

example1 = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

example2 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""

input = readInput 8

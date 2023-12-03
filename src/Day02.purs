module Day02 where

import Prelude
import Parsing (ParseError, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String as P
import Parsing.String.Basic as P
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))
import Utils.Parsing as UP
import Data.List.Types (List)
import Data.Either (Either)

type Game =
  { id :: Int
  , rounds :: List (Map String Int)
  }

parse :: String -> Either ParseError (List Game)
parse s = runParser s $ UP.linesOf game
  where
    game = do
      _ <- P.string "Game "
      id <- P.intDecimal
      _ <- P.string ": "
      rounds <- round `sepBy` P.string "; "
      pure { id, rounds }

    round = Map.fromFoldable <$> (cube `sepBy` P.string ", ")

    cube = do
      n <- P.intDecimal
      _ <- P.string " "
      colour <- UP.word
      pure $ Tuple colour n

example :: String
example = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""


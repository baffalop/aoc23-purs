module Day18 where
-- Lavaduct Lagoon

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (linesOf)
import Parsing.String.Basic (intDecimal) as P
import Parsing.String (char, string) as P
import Utils.Parsing (wordAlphaNum) as P
import Parsing.Combinators (choice) as P
import Data.Tuple.Nested ((/\))
import Data.Tuple (Tuple(Tuple))
import Data.Either (Either)
import Utils.Pointfree ((<<#>>))
import Data.Foldable (foldr)
import Data.Traversable (scanl)
import Control.Bind (join)
import Data.Array as Array
import Data.Set as Set
import Data.Set (Set)
import Data.Map as Map
import Data.Map (Map)
import Data.Foldable as F
import Data.Maybe (fromMaybe)
import Input (readInput)
import Data.Array ((..))

type Vec = Tuple Int Int

type Plan =
  { vec :: Vec
  , color :: String
  }

newtype DebugTrench = DebugTrench (Map Int (Set Int))

solve1 = parse
  <<#>> scanl (\point { vec } -> point + vec) (0 /\ 0)
  >>> foldr (\(x /\ y) -> Map.insertWith (<>) y $ Set.singleton x) (Map.singleton 0 $ Set.singleton 0)
--  >>> map (\xs -> fromMaybe 0 $ do
--    min <- Set.findMin xs
--    max <- Set.findMax xs
--    pure $ max - min + 1
--  )
--  >>> F.sum

parse :: String -> Either ParseError (Array Plan)
parse s = runParser s $ join <$> linesOf do
  vec <- vector <* P.char ' '
  n <- P.intDecimal <* P.char ' '
  color <- P.string "(#" *> P.wordAlphaNum <* P.char ')'
  pure $ Array.replicate n { vec, color }
  where
    vector = P.choice
      [ 0 /\ 1 <$ P.char 'D'
      , 0 /\ -1 <$ P.char 'U'
      , 1 /\ 0 <$ P.char 'R'
      , -1 /\ 0 <$ P.char 'L'
      ]

instance showTrench :: Show DebugTrench where
  show (DebugTrench trench) =
    trench <#> (\coords ->
      F.fold $ (\x -> if x `Set.member` coords then "#" else ".") <$> (xmin .. xmax)
    )
    # F.intercalate "\n"
    where
      allXs = F.fold trench
      xmin = fromMaybe 0 $ Set.findMin allXs
      xmax = fromMaybe 0 $ Set.findMax allXs

example = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""

input = readInput 18

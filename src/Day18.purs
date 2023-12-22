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
import Data.Traversable (class Traversable, scanl)
import Control.Bind (join)
import Data.Array as Array
import Data.Set as Set
import Data.Set (Set)
import Data.Map as Map
import Data.Map (Map)
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromMaybe)
import Input (readInput)
import Data.Array ((..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Control.Alt ((<|>))

type Vec = Tuple Int Int

type Plan =
  { vec :: Vec
  , color :: String
  }

newtype DebugTrench = DebugTrench (Map Int (Set Int))

data Approach = Inside | Outside
derive instance eqLoc :: Eq Approach

inverse :: Approach -> Approach
inverse Inside = Outside
inverse Outside = Inside

solve1 :: String -> Either ParseError Int
solve1 s = do
  trench <- buildTrench <$> parse s
  let
    isDug x y = fromMaybe false do
      xs <- Map.lookup y trench
      pure $ x `Set.member` xs

    sumXs y state@{ prev, adj, approach, sum } x =
      let newState = state { prev = Just x }
      in case prev of
        Nothing -> newState { approach = Inside, sum = sum + 1 }
        Just prevX
          | prevX == x - 1 -> newState { adj = adj <|> Just prevX, sum = sum + 1 }
          | otherwise ->
            let
              nextState = newState { adj = Nothing }
              correctedApproach = case adj of
                Just adjX | isDug adjX (y + 1) == isDug prevX (y + 1) -> inverse approach
                _ -> approach
            in case correctedApproach of
              Inside -> nextState { approach = Outside, sum = sum + x - prevX }
              _ -> nextState { approach = Inside, sum = sum + 1 }

  pure $ F.sum
    $ mapWithIndex (\y -> F.foldl (sumXs y) { prev: Nothing, adj: Nothing, approach: Outside, sum: 0 } >>> _.sum) trench

buildTrench :: forall f. Traversable f => f Plan -> Map Int (Set Int)
buildTrench = scanl (\point { vec } -> point + vec) (0 /\ 0)
  >>> foldr (\(x /\ y) -> Map.insertWith (<>) y $ Set.singleton x) (Map.singleton 0 $ Set.singleton 0)

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

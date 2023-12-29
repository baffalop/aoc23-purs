module Day12 where
-- Hot Springs

import Prelude
import Parsing (ParseError, runParser)
import Utils.Parsing (linesOf)
import Parsing.Combinators (choice, many, sepBy) as P
import Parsing.String (char) as P
import Parsing.String.Basic (intDecimal) as P
import Data.Either (Either)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array as Array
import Data.List as List
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Utils.Pointfree ((<<#>>))
import Input (readInput)
import Data.Foldable as F
import Data.Foldable (class Foldable)

data Spring = Op | Damaged | Unknown
derive instance eqSpring :: Eq Spring

type Conditions = { springs :: List Spring, damaged :: List Int }

instance showSpring :: Show Spring where
  show = case _ of
    Op -> "."
    Damaged -> "#"
    Unknown -> "?"

solve1 :: String -> Either ParseError Int
solve1 = parse
  <<#>> map (\{ springs, damaged } -> countMatches (toContiguous springs) damaged)
  >>> F.sum

solve2 :: String -> Either ParseError Int
solve2 = parse
  <<#>> map (\{ springs, damaged } ->
    countMatches (toContiguous $ unfoldWith (Unknown : Nil) springs) (unfoldWith Nil damaged)
  ) >>> F.sum
  where
    unfoldWith :: forall m. Monoid m => m -> m -> m
    unfoldWith sep = Array.replicate 5 >>> Array.intercalate sep

countMatches :: List (List (Tuple Spring Int)) -> List Int -> Int
countMatches = matches
  where
    matches blocks Nil = if F.any hasDamage blocks then 0 else 1
    matches Nil _ = 0
    matches (Nil : blocks) damaged = matches blocks damaged
    matches (block@(b@(spring /\ _) : restBlock) : restBlocks) damaged@(count : restDamaged)
      | blockLength block < count = if hasDamage block then 0 else matches restBlocks damaged
      | otherwise = case spring /\ consume count block of
        Damaged /\ Nothing -> 0
        Damaged /\ Just consumed -> matches (consumed : restBlocks) restDamaged
        _ /\ Nothing -> matches (reduceBy 1 b restBlock : restBlocks) damaged
        _ /\ Just consumed ->
          matches (consumed : restBlocks) restDamaged
          + matches (reduceBy 1 b restBlock : restBlocks) damaged

    consume 0 Nil = Just Nil
    consume 0 (b@(spring /\ _) : rest) = if spring == Damaged then Nothing else Just $ reduceBy 1 b rest
    consume _ Nil = Nothing
    consume n (b@(spring /\ count) : rest)
      | n < count = if spring == Damaged then Nothing else Just $ reduceBy (n + 1) b rest
      | otherwise = consume (n - count) rest

    reduceBy n (spring /\ count) rest = if n == count then rest else (spring /\ (count - n)) : rest

    blockLength = F.sum <<< map snd
    hasDamage = F.any (fst >>> (_ == Damaged))

toContiguous :: forall f. Foldable f => f Spring -> List (List (Tuple Spring Int))
toContiguous = F.foldr f { cur: Nil, res: Nil } >>> \{ cur, res } -> consIfNonempty cur res
  where
    f spring whole@{ cur, res } = case spring /\ cur of
      Op /\ _ -> { cur: Nil, res: consIfNonempty cur res }
      _ /\ Nil -> { cur: (spring /\ 1) : Nil, res }
      _ /\ (current@(focus /\ count) : others) ->
        whole { cur = if focus == spring then (focus /\ (count + 1)) : others else (spring /\ 1) : current : others }

consIfNonempty :: forall a. List a -> List (List a) -> List (List a)
consIfNonempty Nil ar = ar
consIfNonempty x ar = x : ar

parse :: String -> Either ParseError (Array Conditions)
parse s = runParser s $ linesOf do
  springs <- P.many $ P.choice
    [ Op <$ P.char '.'
    , Damaged <$ P.char '#'
    , Unknown <$ P.char '?'
    ]
  _ <- P.char ' '
  damaged <- P.intDecimal `P.sepBy` P.char ','
  pure { springs, damaged }

example = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""

input = readInput 12

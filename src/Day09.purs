module Day09 where

import Prelude
import Utils.String (lines, words) as String
import Data.Traversable (traverse)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe)
import Data.Array as Array
import Utils.Pointfree ((<<#>>))
import Data.Foldable as F
import Data.Array ((:))
import Input (readInput)

solve1 :: String -> Maybe Int
solve1 = solveWith Array.last F.sum

solve2 :: String -> Maybe Int
solve2 = solveWith Array.head $ F.foldr (-) 0

solveWith :: (Array Int -> Maybe Int) -> (Array Int -> Int) -> String -> Maybe Int
solveWith side extrapolate = parse
  <<#>> map (diffTo0 >>> Array.mapMaybe side >>> extrapolate)
  >>> F.sum

diffTo0 :: Array Int -> Array (Array Int)
diffTo0 xs =
  let deriv = derivative xs
  in if F.all (_ == 0) deriv then [xs] else xs : diffTo0 deriv

derivative :: forall a. Ring a => Array a -> Array a
derivative xs = Array.zipWith (-) (Array.drop 1 xs) xs

parse :: String -> Maybe (Array (Array Int))
parse = String.lines >>> traverse (String.words >>> traverse Int.fromString)

example = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

input = readInput 9

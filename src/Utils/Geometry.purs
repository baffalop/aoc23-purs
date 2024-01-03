module Utils.Geometry where

import Prelude
import Utils.Basics (mapBoth)
import Data.Tuple (Tuple(..), uncurry)
import Data.Ord (abs)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.FunctorWithIndex (mapWithIndex)
import Utils.String (lines) as S
import Data.String.CodeUnits (toCharArray) as S
import Data.Array as Array

type Coord a = Tuple a a

manhattanDistance :: forall a. Ord a => Ring a => Coord a -> Coord a -> a
manhattanDistance c1 c2 = uncurry (+) $ mapBoth abs $ c1 - c2

rotateCw :: forall a. Ring a => Coord a -> Coord a
rotateCw (x /\ y) = -y /\ x

rotateCcw :: forall a. Ring a => Coord a -> Coord a
rotateCcw (x /\ y) = y /\ -x

parseGrid :: forall a. Ord a => (Char -> Maybe a) -> String -> Map (Coord Int) a
parseGrid parseChar = S.lines
  >>> mapWithIndex (\row ->
    S.toCharArray
    >>> mapWithIndex (\col char -> Tuple (col /\ row) <$> parseChar char)
    >>> Array.catMaybes
  )
  >>> join
  >>> Map.fromFoldable


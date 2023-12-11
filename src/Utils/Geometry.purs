module Utils.Geometry where

import Prelude
import Utils.Basics (mapBoth)
import Data.Tuple (Tuple, uncurry)
import Data.Ord (abs)

type Coord a = Tuple a a

manhattanDistance :: forall a. Ord a => Ring a => Coord a -> Coord a -> a
manhattanDistance c1 c2 = uncurry (+) $ mapBoth abs $ c1 - c2


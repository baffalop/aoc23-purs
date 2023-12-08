module Utils.Array where

import Prelude
import PointFree ((<..))
import Data.Array as Array
import Data.Ordering (invert)

sortDesc :: forall a. Ord a => Array a -> Array a
sortDesc = Array.sortBy (invert <.. compare)

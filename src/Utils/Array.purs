module Utils.Array where

import Prelude
import PointFree ((<..))
import Utils.Basics (reverse)
import Data.Array as Array

sortDesc :: forall a. Ord a => Array a -> Array a
sortDesc = Array.sortBy (reverse <.. compare)

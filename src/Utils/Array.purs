module Utils.Array where

import Prelude
import PointFree ((<..))
import Data.Array as Array
import Data.Ordering (invert)
import Data.Maybe (Maybe(..))

sortDesc :: forall a. Ord a => Array a -> Array a
sortDesc = Array.sortBy (invert <.. compare)

infixl 6 Array.snoc as <:

maybeSnoc :: forall a. Array a -> Maybe a -> Array a
maybeSnoc ar Nothing = ar
maybeSnoc ar (Just x) = ar <: x

infixl 6 maybeSnoc as <?:


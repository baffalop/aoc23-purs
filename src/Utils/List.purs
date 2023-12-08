module Utils.List where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List, (:))
import Data.List as List
import PointFree ((<..))
import Data.Ordering (invert)

maybeCons :: forall a. Maybe a -> List a -> List a
maybeCons Nothing = identity
maybeCons (Just x) = (x : _)

infixr 6 maybeCons as ?:

sortDesc :: forall a. Ord a => List a -> List a
sortDesc = List.sortBy (invert <.. compare)

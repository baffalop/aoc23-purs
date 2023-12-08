module Utils.List where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List, (:))
import Data.List as List
import PointFree ((<..))

maybeCons :: forall a. Maybe a -> List a -> List a
maybeCons Nothing = identity
maybeCons (Just x) = (x : _)

infixr 6 maybeCons as ?:

sortDesc :: forall a. Ord a => List a -> List a
sortDesc = List.sortBy (reverse <.. compare)
  where
    reverse = case _ of
      EQ -> EQ
      LT -> GT
      GT -> LT

module Utils.Array where

import Prelude
import PointFree ((<..))
import Data.Array as Array
import Data.Ordering (invert)
import Data.Maybe (Maybe(..))
import Data.Array ((..), (:))
import Data.Either (Either(..))
import Data.Foldable (foldr)

sortDesc :: forall a. Ord a => Array a -> Array a
sortDesc = Array.sortBy (invert <.. compare)

infixl 6 Array.snoc as <:

maybeSnoc :: forall a. Array a -> Maybe a -> Array a
maybeSnoc ar Nothing = ar
maybeSnoc ar (Just x) = ar <: x

infixl 6 maybeSnoc as <?:

rangeAsc :: Int -> Int -> Array Int
rangeAsc from to
  | from <= to = from .. to
  | otherwise = []

infix 8 rangeAsc as ../

partitionMap :: forall a b c. (a -> Either b c) -> Array a -> { left :: Array b, right :: Array c }
partitionMap f = foldr
  (\a res@{ left, right } -> case f a of
    Left b -> res { left = b : left }
    Right c -> res { right = c : right }
  )
  { left: [], right: [] }

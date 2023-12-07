module Utils.Map where

import Prelude

import Data.Map as Map
import Data.Map (Map)
import Data.Foldable (class Foldable, foldr)

counts :: forall a f. Ord a => Foldable f => f a -> Map a Int
counts = foldr (\x -> Map.insertWith (+) x 1) Map.empty

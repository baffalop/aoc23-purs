module Utils.Map where

import Prelude

import Data.Map as Map
import Data.Map (Map)
import Data.Foldable (class Foldable, foldr)
import Data.Tuple (Tuple)
import Data.Tuple (swap) as Tuple

counts :: forall a f. Ord a => Foldable f => f a -> Map a Int
counts = foldr (\x -> Map.insertWith (+) x 1) Map.empty

invert :: forall a b. Ord a => Ord b => Map a b -> Map b a
invert m = Map.fromFoldable $ Tuple.swap <$> (Map.toUnfoldable m :: Array (Tuple a b))

module Utils.Basics where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Tuple (Tuple(..))

mapBoth :: forall a b f. Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = bimap f f

dup :: forall a. a -> Tuple a a
dup x = Tuple x x

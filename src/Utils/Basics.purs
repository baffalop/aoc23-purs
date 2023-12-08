module Utils.Basics where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)

mapBoth :: forall a b f. Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = bimap f f

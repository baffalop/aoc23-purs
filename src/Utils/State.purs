module Utils.State where

import Prelude
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Foldable (class Foldable)
import Uncurried.State (State, runState)
import Data.Foldable as F

foldrState :: forall f s a b. Foldable f => (a -> b -> State s b) -> s -> b -> f a -> b
foldrState f initS initV = fst <<< F.foldr (\v (r /\ s) -> runState s $ f v r) (initV /\ initS)


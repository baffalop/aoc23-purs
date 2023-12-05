module Utils.List where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List, (:))

maybeCons :: forall a. Maybe a -> List a -> List a
maybeCons Nothing = identity
maybeCons (Just x) = (x : _)

infixr 6 maybeCons as ?:

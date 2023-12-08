module Utils.Basics where

import Prelude

reverse :: Ordering -> Ordering
reverse = case _ of
  EQ -> EQ
  LT -> GT
  GT -> LT

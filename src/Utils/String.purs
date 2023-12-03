module Utils.String where

import Prelude
import Data.String as S
import Data.String.Pattern (Pattern(Pattern))

lines :: String -> Array String
lines = S.split (Pattern "\n")

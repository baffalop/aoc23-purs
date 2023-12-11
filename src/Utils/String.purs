module Utils.String where

import Prelude
import Data.String as S
import Data.String.Pattern (Pattern(Pattern))
import Data.String.Regex as R
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags) as RF
import Partial.Unsafe (unsafeCrashWith)
import Data.String.Regex.Flags (RegexFlags)
import PointFree ((<..))
import Data.Either (either)
import Data.Unfoldable (unfoldr)
import Data.Tuple (Tuple(Tuple))

lines :: String -> Array String
lines = S.split (Pattern "\n")

words :: String -> Array String
words = R.split $ regexSimple "\\s+"

regexSimple :: String -> Regex
regexSimple = flip regexUnsafe RF.noFlags

regexUnsafe :: String -> RegexFlags -> Regex
regexUnsafe = either unsafeCrashWith identity <.. R.regex

allIndicesOf :: Pattern -> String -> Array Int
allIndicesOf pattern s = 0 # unfoldr \lastIndex -> do
  index <- S.indexOf' pattern lastIndex s
  pure $ Tuple index (index + 1)

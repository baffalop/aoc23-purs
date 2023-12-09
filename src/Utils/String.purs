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

lines :: String -> Array String
lines = S.split (Pattern "\n")

words :: String -> Array String
words = R.split whitespaceRegex

whitespaceRegex :: Regex
whitespaceRegex = unsafeRegex "\\s+" RF.noFlags

unsafeRegex :: String -> RegexFlags -> Regex
unsafeRegex = either unsafeCrashWith identity <.. R.regex

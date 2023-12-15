module Day15 where
-- Lens Library

import Prelude
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(..))
import Data.Char (toCharCode) as Char
import Data.String.CodeUnits (toCharArray) as String
import Data.Foldable as F
import Input (readInput)

solve1 :: String -> Int
solve1 = String.split (Pattern ",") >>> map hash >>> F.sum

hash :: String -> Int
hash = String.toCharArray >>> F.foldl addHash 0
  where
    addHash h c = Char.toCharCode c # (_ + h) # (_ * 17) # (_ `mod` 256)

example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

input = readInput 15

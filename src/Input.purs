module Input (readInput) where

import Prelude
import Effect (Effect)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

readInput :: Int -> Effect String
readInput day =
  readTextFile UTF8 $ "input/" <> pad0 day <> ".txt"

pad0 :: Int -> String
pad0 n = (if n < 10 then "0" else "") <> show n

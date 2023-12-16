module Day15 where
-- Lens Library

import Prelude
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(..))
import Data.Char (toCharCode) as Char
import Data.String.CodeUnits (toCharArray) as String
import Data.Foldable as F
import Input (readInput)
import Data.Either (Either)
import Parsing (ParseError, runParser)
import Utils.Parsing (arraySepBy)
import Parsing.String (char) as P
import Utils.Parsing (word) as P
import Parsing.Combinators (choice) as P
import Parsing.String.Basic (intDecimal) as P
import Utils.Pointfree ((<<#>>))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Alt ((<|>))
import Utils.Array ((<:))
import Data.FoldableWithIndex (foldlWithIndex)

type Instruction =
  { label :: String
  , op :: Op
  }

data Op = Remove | Place Int

instance showOp :: Show Op where
  show Remove = "Remove"
  show (Place f) = "Place " <> show f

type Lens =
  { label :: String
  , focalLength :: Int
  }

solve1 :: String -> Int
solve1 = String.split (Pattern ",") >>> map hash >>> F.sum

solve2 :: String -> Either ParseError Int
solve2 = parse <<#>> foldl doInstruction Map.empty >>> focusingPower
  where
    doInstruction :: Map Int (Array Lens) -> Instruction -> Map Int (Array Lens)
    doInstruction boxes { label, op } =
      let box = hash label
      in case op of
        Remove -> Map.update (Just <<< Array.filter (_.label >>> (_ /= label))) box boxes
        Place focalLength -> Map.alter (Just <<< insertLens <<< fromMaybe []) box boxes
          where
            lens = { label, focalLength }
            insertLens lenses = fromMaybe (lenses <: lens) $ updateLens lenses
            updateLens lenses = do
              i <- Array.findIndex (_.label >>> (_ == label)) lenses
              Array.updateAt i lens lenses

    focusingPower :: Map Int (Array Lens) -> Int
    focusingPower = 0 # foldlWithIndex \box total lenses ->
      total + foldlWithIndex (\i boxTotal { focalLength } -> boxTotal + (box + 1) * (i + 1) * focalLength) 0 lenses

parse :: String -> Either ParseError (Array Instruction)
parse s = runParser s $ instruction `arraySepBy` P.char ','
  where
    instruction = { label: _, op: _} <$> P.word <*> P.choice
      [ Remove <$ P.char '-'
      , Place <$> (P.char '=' *> P.intDecimal)
      ]

hash :: String -> Int
hash = String.toCharArray >>> F.foldl addHash 0
  where
    addHash h c = Char.toCharCode c # (_ + h) # (_ * 17) # (_ `mod` 256)

example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

input = readInput 15

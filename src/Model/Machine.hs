module Model.Machine
  ( Tape (..),
    Machine (..),
    pfTape,
  )
where

-- import Data.Model (Program)

import Model.Program (Program)
import PyF (fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)

data Tape = Tape
  { left :: [Char],
    cursor :: Char,
    right :: [Char]
  }

pfTape :: Tape -> [Char]
pfTape (Tape l h r) =
  let ann = color Red . style Bold
   in [fmtTrim|\
     {l ++ ann [h] ++ r}
     {ann "^":<{length l}}
   |]

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String
  }

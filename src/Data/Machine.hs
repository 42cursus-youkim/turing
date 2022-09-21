module Data.Machine (Tape (..), Machine (..)) where

import Data.Model (Program)

data Tape = Tape
  { left :: [Char],
    head :: Char,
    right :: [Char]
  }

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String
  }

instance Show Tape where
  show (Tape l h r) = l ++ [h] ++ r

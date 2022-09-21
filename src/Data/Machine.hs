module Data.Machine (Tape (..), Machine (..)) where

import Data.Model (Program)
import System.Console.Pretty (Color (..), Style (..), color, style)

data Tape = Tape
  { left :: [Char],
    cursor :: Char,
    right :: [Char]
  }

instance Show Tape where
  show :: Tape -> String
  show (Tape l h r) = l ++ curs ++ r
    where
      curs = color Red . style Bold $ [h]

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String
  }

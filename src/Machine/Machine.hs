module Machine.Machine
  ( Machine (..),
  )
where

import Machine.Tape (Tape)
import qualified Machine.Tape as T
import Model.Program (Program (initial))
import PyF (fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String,
    stuck :: Bool
  }

-- | Assumes are inputs are correct.
initMachine :: String -> Program -> Machine
initMachine s p =
  Machine
    { tape = T.fromString s,
      program = p,
      state = initial p,
      stuck = False
    }

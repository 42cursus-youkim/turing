module Machine.Machine
  ( Machine (..),
    initMachine,
    pprintMachine,
    runMachine,
  )
where

import qualified Data.Map as M
import Machine.Tape (Tape, moveTape, pfTape, pfTapeLong)
import qualified Machine.Tape as T
import Model.Action (Direction (..))
import Model.Program (Program (blank, finals, initial, transitions))
import PyF (fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)

data MachineState = Running | Stuck | Finished deriving (Show, Eq)

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String,
    stuck :: MachineState
  }

pprintMachine :: Machine -> IO ()
pprintMachine m = do
  putStrLn
    [fmtTrim|
      Tape: {pfTape (tape m)}
      State: {state m}
      Stuck: {stuck m:s}|]

-- | Assumes that inputs are correct.
initMachine :: String -> Program -> Machine
initMachine s p =
  Machine
    { tape = T.initTape s (blank p),
      program = p,
      state = initial p,
      stuck = Running
    }

runMachine :: Machine -> IO ()
runMachine m = do
  let res = step m
  pprintMachine res

step :: Machine -> Machine
step m
  | ok = m {tape = moveTape (tape m) ToLeft}
  | finished = m {stuck = Finished}
  | otherwise = m {stuck = Stuck}
  where
    p = program m
    ok = state m `M.member` transitions p
    finished = stuck m == Finished || state m `elem` finals p




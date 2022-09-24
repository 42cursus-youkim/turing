module Machine.Machine
  ( Machine (..),
    -- runMachine,
    initMachine,
    -- runMachine,
    pprintMachine,
    -- runMachine,
    runMachine,
  )
where

import qualified Data.Map as M
import Machine.Tape (Tape, pfTape, pfTapeLong)
import qualified Machine.Tape as T
import Model.Program (Program (finals, initial, transitions))
import PyF (fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String,
    stuck :: Bool
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
    { tape = T.initTape s,
      program = p,
      state = initial p,
      stuck = False
    }

runMachine :: Machine -> IO ()
runMachine m = do
  let res = step m
  pprintMachine res



step :: Machine -> Machine
step m
  | ok = m
  | finished = m
  | otherwise = m {stuck = True}
  where
    p = program m
    ok = state m `elem` M.keys (transitions p)
    finished = state m `elem` finals p

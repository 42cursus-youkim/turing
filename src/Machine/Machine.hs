module Machine.Machine
  ( Machine (..),
    initMachine,
    pprintMachine,
    runMachine,
  )
where

import Control.Monad (when)
import Data.List (find)
import qualified Data.Map as M
import Machine.Tape (Tape (..), moveTape, pfTape, pfTapeLong)
import qualified Machine.Tape as T
import Model.Action (Action (read_, to_state), Direction (..))
import qualified Model.Action as A
import Model.Program (Program (finals, initial, transitions))
import qualified Model.Program as P
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
pprintMachine m =
  putStrLn
    [fmtTrim|{pfTape (tape m)} {state m} {stuck m:s}|]

-- | Assumes that inputs are correct.
initMachine :: String -> Program -> Machine
initMachine s p =
  Machine
    { tape = T.initTape s (P.blank p),
      program = p,
      state = initial p,
      stuck = Running
    }

runMachine :: Machine -> IO ()
runMachine m = do
  let res = step m
  pprintMachine res
  when (stuck res == Running) $ runMachine res

step :: Machine -> Machine
step m
  | ok = runAction m
  | finished = m {stuck = Finished}
  | otherwise = m {stuck = Stuck}
  where
    p = program m
    ok = state m `M.member` transitions p
    finished = stuck m == Finished || state m `elem` finals p

findRead :: [Action] -> Char -> Maybe Action
findRead xs curs = find (\x -> read_ x == curs) xs

applyTape :: Tape -> Action -> Tape
applyTape t a = moveTape (T.writeTape t (A.write a)) (A.action a)

runAction :: Machine -> Machine
runAction m =
  case state m `M.lookup` transitions p of
    Nothing -> m {stuck = Stuck}
    Just xs -> case findRead xs (T.cursor (tape m)) of
      Nothing -> m {stuck = Stuck}
      Just act ->
        m
          { tape = applyTape (tape m) act,
            state = to_state act
          }
  where
    p = program m

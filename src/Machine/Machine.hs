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
import Debug.Trace (trace)
import Machine.Tape (Tape (..), moveTape, pfTape, pfTapeLong)
import qualified Machine.Tape as T
import Model.Action (Action (read_, to_state), Direction (..))
import qualified Model.Action as A
import Model.Program (Program (finals, initial, transitions))
import qualified Model.Program as P
import PyF (fmt, fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)
import Util (boldCol, termWidth)

data MachineState = Running | Stuck | Finished deriving (Show, Eq)

data Machine = Machine
  { tape :: Tape,
    program :: Program,
    state :: String,
    stuck :: MachineState
  }

pprintMachine :: Machine -> Int -> IO ()
pprintMachine m w =
  putStrLn
    [fmtTrim|{pfTape (tape m):<{w * 2 `div` 3 + 9}} {boldCol Magenta (state m)}|]

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
  w <- termWidth
  case stuck m of
    Stuck -> putStrLn "Machine is stuck!"
    Finished -> return ()
    Running -> do
      pprintMachine m w
      runMachine (step m)

step :: Machine -> Machine
step m
  | finished = m {stuck = Finished}
  | otherwise = runAction m
  where
    p = program m
    finished = state m `elem` finals p

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

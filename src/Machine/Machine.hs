module Machine.Machine
  ( Machine (..),
    initMachine,
    pprintMachine,
    logfHistory,
    runMachine,
    pprintHistory,
    benchmarkInputs,
  )
where

import Control.Monad (when)
import Data.List (find)
import qualified Data.Map as M
import Debug.Trace (trace)
import Machine.Tape (Tape (..), moveTape, pfTape)
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
  deriving (Show)

pfMachine :: Machine -> Int -> String
pfMachine m w =
  [fmtTrim|{pfTape (tape m):<{w * 2 `div` 3 + 9}} {boldCol Magenta (state m)}|]

pprintMachine :: Machine -> Int -> IO ()
pprintMachine m w = putStrLn $ pfMachine m w

-- | Assumes that inputs are correct.
initMachine :: Program -> String -> Machine
initMachine p s =
  Machine
    { tape = T.initTape s (P.blank p),
      program = p,
      state = initial p,
      stuck = Running
    }

runMachine :: Machine -> [Machine]
runMachine m = takeWhile (\x -> stuck x == Running) (iterate step m)

benchmarkInput :: Machine -> (Int, Int)
benchmarkInput m =
  let result = runMachine m
      inputLength = T.tapeSize $ tape m
      step' = length result
   in (inputLength, step')

benchmarkInputs :: Program -> [String] -> [(Int, Int)]
benchmarkInputs p = map (benchmarkInput . initMachine p)

lastState :: [Machine] -> MachineState
lastState = stuck . step . last

pprintHistory :: [Machine] -> IO ()
pprintHistory history = do
  w <- termWidth
  mapM_ (pprintMachine <*> pure w) history
  case lastState history of
    Stuck -> putStrLn $ boldCol Red "Machine is Stuck"
    Finished -> putStrLn $ boldCol Green "Machine is Finished"
    _ -> return ()

logfHistory :: [Machine] -> String
logfHistory history = do
  [fmtTrim|{p:s}\n{unlines tapes}|]
  where
    p = program $ head history
    logfTape (Tape l h r _) = [fmt|[{l}{h}{r}]|]
    fn h = [fmt|{logfTape t} {state h}\n {'^':>{length (left t)}}|] where t = tape h
    tapes = map fn history

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

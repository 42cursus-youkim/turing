module Main where

import Arg.Args
import Control.Monad (unless, when)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Machine.Machine (Machine (..), benchmarkInputs, initMachine, logfHistory, pprintHistory, runMachine)
import Machine.Tape (pfTape)
import Model.Program (Program (Program), pprintProgram)
import Model.Reader (readProgram)
import Options.Applicative (execParser)
import PyF (fmt)
import Util (termWidth)

runSingle :: Program -> String -> CommonOpts -> IO ()
runSingle p input (CommonOpts _ silent logging) = do
  let history = runMachine $ initMachine p input
  unless silent do
    pprintProgram p
    pprintHistory history
  case logging of
    Just f -> writeFile f (logfHistory history)
    Nothing -> pure ()

runMultiple :: Program -> GraphOpts -> IO ()
runMultiple p (GraphOpts input _output) = do
  tapes <- readFile input
  let inputs = lines tapes
      result = benchmarkInputs p inputs
  print result -- TODO: save svg graph

parseInput :: Args -> IO ()
parseInput (Args opt input) = do
  readProgram (instruction opt) >>= \case
    Left e -> putStrLn [fmt|read failed with: {e:s}|]
    Right p -> case input of
      Run tapeText -> runSingle p tapeText opt
      Graph gopts -> runMultiple p gopts

main :: IO ()
main = execParser opts >>= parseInput

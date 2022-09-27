module Main where

import Arg.Args
  ( Args (..),
    CommonOpts (..),
    args,
    opts,
  )
import Arg.FileArgs (FileArgs (GrapOpts), GraphOpts (..))
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

-- runSingle :: String -> Program -> IO ()
-- runSingle input p = do
--   let history = runMachine $ initMachine p input
--   unless silent do
--     pprintProgram p
--     pprintHistory history
--   case logging of
--     Just f -> writeFile f (logfHistory history)
--     Nothing -> pure ()

runMultiple :: Program -> GraphOpts -> IO ()
runMultiple p (GraphOpts input output) = do
  tapes <- readFile input
  let inputs = lines tapes
      result = benchmarkInputs p inputs
  print result

-- runFrom :: Args -> IO ()
-- runFrom (Args (GraphOpts tapesFile graphFile)) = do

parseInput :: Args -> IO ()
parseInput (Args (CommonOpts file silent logging) input) = do
  readProgram file >>= \case
    Left e -> putStrLn [fmt|read failed with: {e:s}|]
    Right p -> runMultiple p input

main :: IO ()
main = execParser opts >>= parseInput

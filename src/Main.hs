module Main where

import Args
  ( Args (..),
    CommonOpts (..),
    args,
    opts,
  )
import Control.Monad (unless)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Machine.Machine (Machine (..), initMachine, pprintHistory, runMachine)
import Machine.Tape (pfTape)
import Model.Program (pprintProgram)
import Model.Reader (readProgram)
import Options.Applicative (execParser)
import PyF (fmt)
import Util (termWidth)

runFrom :: Args -> IO ()
runFrom (Args (CommonOpts silent _ file) input) = do
  readProgram file >>= \case
    Left e -> putStrLn [fmt|read failed with: {e:s}|]
    Right p -> do
      let history = runMachine $ initMachine input p
      unless silent do
        pprintProgram p
        pprintHistory history

main :: IO ()
main = execParser opts >>= runFrom

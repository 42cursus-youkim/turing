{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Machine.Machine (Machine (..), initMachine, pprintHistory, runMachine)
import Machine.Tape (pfTape)
import Model.Program (pprintProgram)
import Model.Reader (readProgram, testFile)
import Options.Applicative (execParser)
import PyF (fmt)
import TuringArgs (TuringArgs (TuringArgs), opts)
import Util (termWidth)

runFrom :: TuringArgs -> IO ()
runFrom (TuringArgs file input quiet) = do
  readProgram file >>= \case
    Left e -> putStrLn [fmt|read failed with: {e:s}|]
    Right p -> do
      let history = runMachine $ initMachine input p
      unless quiet do
        pprintProgram p
        pprintHistory history

main :: IO ()
main = execParser opts >>= runFrom

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Machine.Machine (Machine (..), initMachine, pprintMachine, runMachine)
import Machine.Tape (pfTape)
import Model.Program (pprintProgram)
import Model.Reader (readProgram, testFile)

main :: IO ()
main = do
  readProgram testFile >>= \case
    Left e -> print e
    Right p -> do
      pprintProgram p
      runMachine (initMachine "111-11=" p)

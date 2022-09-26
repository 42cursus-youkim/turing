{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Machine.Machine (Machine (..), initMachine, pprintMachine, runMachine)
import Machine.Tape (pfTape)
import Model.Program (pprintProgram)
import Model.Reader (readProgram, testFile)
import Options.Applicative
import TuringArgs
import Util (termWidth)

utm :: String
utm = ">010101100111$00-01|00-00-00-00-1|00-01-00-01-1|00-10-00-10-1|00-11-01-00-0|01-01-10-00-0|01-10-11-00-0|10-01-10-01-0|10-10-11-01-0$"

runFrom :: TuringArgs -> IO ()
runFrom (TuringArgs file input) = do
  readProgram file >>= \case
    Left e -> print e
    Right p -> do
      pprintProgram p
      let m = initMachine input p
      runMachine m

main :: IO ()
main = execParser opts >>= runFrom

-- main :: IO ()
-- main =
--   runFrom "docs/examples/utm.json" utm

-- runFrom "docs/examples/unary_add.json" "111+11="

module Main where

import Arg.Args
  ( Args (..),
    CommonOpts (..),
    args,
    opts,
  )
import Control.Monad (unless, when)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Machine.Machine (Machine (..), initMachine, logfHistory, pprintHistory, runMachine)
import Machine.Tape (pfTape)
import Model.Program (pprintProgram)
import Model.Reader (readProgram)
import Options.Applicative (execParser)
import PyF (fmt)
import Util (termWidth)

runFrom :: Args -> IO ()
runFrom (Args (CommonOpts file silent logging) input) = do
  readProgram file >>= \case
    Left e -> putStrLn [fmt|read failed with: {e:s}|]
    Right p -> do
      let history = runMachine $ initMachine input p
      unless silent do
        pprintProgram p
        pprintHistory history
      case logging of
        Just f -> writeFile f (logfHistory history)
        Nothing -> pure ()

main :: IO ()
main = execParser opts >>= runFrom

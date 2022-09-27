module Arg.TapeInput (TapeInput, tapeInput) where

import Arg.FileArgs (FileArgs (..), TapeInput)
import Options.Applicative

tapeInput :: Parser TapeInput
tapeInput = argument str (metavar "TAPE" <> help "input tape")

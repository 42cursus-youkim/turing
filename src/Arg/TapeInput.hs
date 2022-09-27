module Arg.TapeInput (TapeInput, tapeInput) where

import Options.Applicative

type TapeInput = String

tapeInput :: Parser TapeInput
tapeInput = argument str (metavar "TAPE" <> help "input tape")

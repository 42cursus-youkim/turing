module Arg.Args
  ( Args (..),
    CommonOpts (..),
    args,
    opts,
  )
where

import Arg.CommonOpts
import Arg.TapeInput
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative

data Args = Args CommonOpts TapeInput
  deriving (Show)

args :: Parser Args
args = Args <$> commonOpts <*> tapeInput

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    ( fullDesc
        <> progDesc "Run a turing machine or generate performance graph"
        <> header "exciting-turing - a Turing machine simulator"
    )

{-
GRAPH: Graph generation mode
graph JSON_FILE FILE_INPUT: Generate a svg graph of the program
--out, -o FILE_OUTPUT: Output svg path for the graph (default CWD)

Both modes
--quiet, -q: Do not print the program output
--log, -l FILE_OUTPUT: Output log file
-}

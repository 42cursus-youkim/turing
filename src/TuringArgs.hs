module TuringArgs
  ( opts,
    turingArgs,
    TuringArgs (..),
  )
where

import Data.List
import Data.Semigroup ((<>))
import Options.Applicative

data TuringArgs = TuringArgs
  { instructions :: FilePath,
    tapeInput :: String
  }
  deriving (Show)

turingArgs :: Parser TuringArgs
turingArgs =
  TuringArgs
    <$> argument str (metavar "FILE" <> help "json description of the machine")
    <*> argument str (metavar "TAPE" <> help "input of the machine")

opts :: ParserInfo TuringArgs
opts =
  info
    (turingArgs <**> helper)
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

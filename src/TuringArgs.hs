module TuringArgs where

import Data.List
import Data.Semigroup ((<>))
import Options.Applicative

data Args = Args CommonOpts Target
  deriving (Show)

newtype CommonOpts = CommonOpts {quiet :: Bool}
  deriving (Show)

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts <$> switch (long "quiet" <> short 'q' <> help "do not print result")

data Target = Target {instructions :: FilePath, tapeInput :: String}
  deriving (Show)

target :: Parser Target
target =
  Target
    <$> argument str (metavar "FILE" <> help "json description of the machine")
    <*> argument str (metavar "TAPE" <> help "input tape")

args :: Parser Args
args = Args <$> commonOpts <*> target

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

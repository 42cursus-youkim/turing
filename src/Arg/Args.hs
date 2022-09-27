module Arg.Args
  ( Args (..),
    CommonOpts (..),
    args,
    opts,
  )
where

import Arg.CommonOpts
import Arg.FileArgs (FileArgs, GraphOpts)
import Arg.Graph (graph)
import Arg.TapeInput
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative

data Args = Args CommonOpts GraphOpts
  deriving (Show)

-- fileArgs :: Parser FileArgs
-- fileArgs = tapeInput <|> graph

args :: Parser Args
args = Args <$> commonOpts <*> graph

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    ( fullDesc
        <> progDesc "Run a turing machine or generate performance graph"
        <> header "exciting-turing - a Turing machine simulator"
    )

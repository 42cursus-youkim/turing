module Arg.Args
  ( Args (..),
    CommonOpts (..),
    GraphOpts (..),
    Command (..),
    args,
    opts,
  )
where

import Arg.Command
import Arg.CommonOpts
import Arg.Opts
import Arg.TapeInput
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative

data Args = Args CommonOpts Command
  deriving (Show)

args :: Parser Args
args = Args <$> commonOpts <*> commands

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    ( fullDesc
        <> progDesc "Run a turing machine or generate performance graph"
        <> header "exciting-turing - a Turing machine simulator"
    )

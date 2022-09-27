module Arg.Graph (GraphOpts (..), graphOpts) where

import Arg.Opts (GraphOpts (..))
import Options.Applicative

graphOpts :: Parser GraphOpts
graphOpts =
  GraphOpts
    <$> argument
      str
      ( metavar "FILE"
          <> help "tapes input file delimited with newlines"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> value "graph.svg"
          <> help "Path to save Graph svg output"
      )

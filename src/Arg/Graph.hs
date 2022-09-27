module Arg.Graph (GraphOpts (..), graph) where

import Arg.FileArgs (FileArgs (..), GraphOpts (..))
import Options.Applicative

graph :: Parser GraphOpts
graph =
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

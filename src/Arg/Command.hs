module Arg.Command where

import Arg.Graph
import Arg.Opts
import Arg.TapeInput
import Options.Applicative

runCommand :: Parser Command
runCommand = Run <$> tapeInput

graphCommand :: Parser Command
graphCommand = Graph <$> graphOpts

commands :: Parser Command
commands =
  subparser
    ( command
        "run"
        ( info
            runCommand
            ( progDesc
                "run turing machine with single input"
            )
        )
    )
    <|> subparser
      ( command
          "graph"
          ( info
              graphCommand
              ( progDesc
                  "generate a svg graph"
              )
          )
      )

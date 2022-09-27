module Arg.CommonOpts (commonOpts) where

import Arg.Opts
import Options.Applicative

commonOpts :: Parser CommonOpts
commonOpts =
  CommonOpts
    <$> argument
      str
      ( metavar "FILE"
          <> help "json description of the machine"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "do not print result"
      )
    <*> optional
      ( strOption
          ( long "log"
              <> short 'l'
              <> metavar "FILE"
              <> help "save log file to path"
          )
      )

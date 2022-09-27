module Arg.CommonOpts (CommonOpts (..), commonOpts) where

import Options.Applicative

data CommonOpts = CommonOpts
  { instruction :: FilePath,
    quiet :: Bool,
    logFile :: Maybe FilePath
  }
  deriving (Show)

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

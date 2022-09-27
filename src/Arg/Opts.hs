module Arg.Opts
  ( CommonOpts (..),
    GraphOpts (..),
    Command (..),
    TapeInput,
  )
where

type TapeInput = String

data GraphOpts = GraphOpts
  { tapesFile :: FilePath,
    graphFile :: FilePath
  }
  deriving (Show)

data CommonOpts = CommonOpts
  { instruction :: FilePath,
    quiet :: Bool,
    logFile :: Maybe FilePath
  }
  deriving (Show)

data Command
  = Run TapeInput
  | Graph GraphOpts
  deriving (Show)

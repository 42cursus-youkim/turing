module Arg.FileArgs where

type TapeInput = String

data GraphOpts = GraphOpts
  { tapesFile :: FilePath,
    graphFile :: FilePath
  }
  deriving (Show)

data FileArgs
  = TapeInput
  | GrapOpts GraphOpts
  deriving (Show)

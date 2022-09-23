{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, encode, fromJSON)
import qualified Data.ByteString.Lazy as B
import Data.Either
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Model.Action
import Model.Program
import Util (note)

testFile :: FilePath
testFile = "docs/examples/unary_sub.json"

getJSON :: IO B.ByteString
getJSON = B.readFile testFile

data ProgramError = FileError | ParseError deriving (Show)

readProgram :: FilePath -> IO (Either ProgramError Program)
readProgram path = do
  note ParseError . decode <$> B.readFile path

main :: IO ()
main = do
  readProgram testFile >>= \case
    Left e -> print e
    Right program -> do
      pprintProgram program

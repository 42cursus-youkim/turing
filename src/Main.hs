{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, encode, fromJSON)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Model.Action
import Model.Program
import Util (header)

testFile :: FilePath
testFile = "docs/examples/unary_sub.json"

getJSON :: IO B.ByteString
getJSON = B.readFile testFile

main :: IO ()
main = do
  j <- getJSON
  let d = fromJust (decode j :: Maybe Program)
  print d

-- print $ encode d
-- where

-- putStrLn $ showAction "scanright" d

-- case d of
-- Nothing -> putStrLn "Error parsing JSON"

module Main where

import Data.Model
import PyF (fmt)
import Util (termWidth)

main :: IO ()
main = do
  -- readFile "docs/examples/unary_sub.json" >>= print
  -- print str
  -- print act
  termWidth >>= print
  where
    str = [fmt|{hello}, {world}|] :: String
    hello = "Hello" :: String
    world = "World" :: String
    act = Action {read_ = '.', to_state = "scanright", write = '.', action = "RIGHT"}

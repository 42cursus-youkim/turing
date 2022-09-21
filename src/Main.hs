{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Machine (Tape (..))
import Data.Model
import GHC.TypeLits (ErrorMessage (Text))
import PyF (fmt)
import Util (header, termWidth, unindent)

main :: IO ()
main = do
  -- readFile "docs/examples/unary_sub.json" >>= print
  -- print str
  -- print act
  -- termWidth >>= print
  -- print tape
  -- print act
  -- print act2
  header "unary_sub"
  -- print $ unindent "  hello\n  world"
  where
    -- str = [fmt|{hello}, {world}|] :: String
    -- hello = "Hello"
    -- world = "World"
    act = Action {read_ = '.', to_state = "scanright", write = '.', action = "RIGHT"}
    act2 = Action {read_ = '.', to_state = "scanright", write = '1', action = "LEFT"}
    tape = Tape {left = "111", cursor = '1', right = "000"}

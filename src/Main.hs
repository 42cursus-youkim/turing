module Main where

import PyF (fmt)

main :: IO ()
main = putStrLn [fmt|Hello, {name}!!|]
  where
    name = "World"

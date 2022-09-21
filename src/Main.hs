module Main where

import qualified Data.Map as Map
import PyF (fmt)

data Action = Action
  { read :: Char,
    to_state :: String,
    write :: Char,
    action :: String
  }
  deriving (Eq, Show)

data Program = Program
  { name :: String,
    alphabet :: [Char],
    blank :: Char,
    states :: [String],
    initial :: String,
    final :: [String],
    transitions :: Map.Map String Action
  }
  deriving (Show)

main :: IO ()
main =
  putStrLn [fmt|{hello}, {world}|]
  where
    hello = "Hello"
    world = "World"

-- where
-- act = Action {read = 'a', to_state = "q1", write = 'b', action = "R"}

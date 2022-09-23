module Model.Program (Program (..), pprintProgram) where

import Data.Aeson
import Data.Map (Map)
import GHC.Generics (Generic)
import Model.Action (Action)
import PyF (fmtTrim)
import System.Console.Terminal.Size (Window (width))
import Util

type Transitions = Map String [Action]

data Program = Program
  { name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Transitions
  }
  deriving (Generic, Show, Eq)

instance FromJSON Program

instance ToJSON Program

pfHeader :: String -> Int -> String
pfHeader n w =
  let line = replicate w '*'
      slugged = capitalize . slugify $ n
   in [fmtTrim|
      {line}
      *{slugged:^{w - 2}}*
      {line}|]

pprintProgram :: Program -> IO ()
pprintProgram (Program n a b s i f t) = do
  termWidth >>= putStrLn . pfHeader n
  putStrIndent
    [fmtTrim|
    Alphabet: {a:s}
    Blank: {b:s}
    States: {s:s}
    Initial: {i:s}
    Finals: {f:s}
    Transitions: {t:s}|]

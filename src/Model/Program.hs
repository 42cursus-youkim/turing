module Model.Program (Program (..), pprintProgram) where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Model.Action (Action, pfAction)
import PyF (fmt, fmtTrim)
import System.Console.Pretty (Color (..), Pretty (color))
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

pfTransition :: String -> [Action] -> String
pfTransition state xs = [fmt|{boldCol Magenta state}\n{indent 2 s}\n|]
  where
    s = unlines $ map (pfAction state) xs

pfTransitions :: Transitions -> String
pfTransitions xs = concatMap (uncurry pfTransition) $ M.toList xs

pfHeader :: String -> Int -> String
pfHeader n w =
  let line = replicate w '*'
      slugged = capitalize . slugify $ n
   in boldCol
        Blue
        [fmtTrim|
      {line}
      *{' ':^{w-2}}*
      *{slugged:^{w - 2}}*
      *{' ':^{w-2}}*
      {line}|]

pprintProgram :: Program -> IO ()
pprintProgram (Program n a b s i f t) = do
  
  putStrLn . pfHeader n =<< termWidth
  putStrIndent
    [fmtTrim|\n
    Alphabet: {a:s}
    Blank: {b:s}
    States: {s:s}
    Initial: {i:s}
    Finals: {f:s}
    Transitions:\n\n|]
  putStrLn $ pfTransitions t

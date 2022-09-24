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

pfTransition :: String -> [Action] -> String
pfTransition state xs =
  [fmt|{boldCol Magenta state} {{\n{indent 2 s}}}\n\n|]
  where
    s = unlines $ map (pfAction state) xs

pfTransitions :: Transitions -> String
pfTransitions xs = concatMap (uncurry pfTransition) $ M.toList xs

pfHeader :: String -> Int -> String
pfHeader n w =
  let slugged = capitalize . slugify $ n
      side = [fmt|*{' ':^{w-2}}*\n|]
      line = replicate w '*' ++ "\n"
   in boldCol Blue [fmt|{line}{side}*{slugged:^{w - 2}}*{side}{line}|]

pprintProgram :: Program -> IO ()
pprintProgram (Program n a b s i f t) = do
  putStrLn . pfHeader n =<< termWidth
  putStrLn $ unlines $ zipWith fn keys values
  putStrLn $ pfTransitions t
  where
    fn :: String -> String -> String
    fn k v = [fmt|{boldCol Cyan k}: {color Yellow v}|]
    keys = ["Alphabet", "Blank", "States", "Initial", "Finals", "Transitions"]
    values = [show a, b, show s, i, show f, ""]

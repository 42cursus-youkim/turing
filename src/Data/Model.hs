module Data.Model
  ( Action (..),
    Program (..),
  )
where

import Data.Aeson
import qualified Data.Map as Map
import PyF (fmt)
import System.Console.Pretty (Color (..), Style (..), color, style)
import Util

data Action = Action
  { read_ :: Char,
    to_state :: String,
    write :: Char,
    action :: String
  }
  deriving (Eq)

instance Show Action where
  show = showAction ""

showAction :: String -> Action -> String
showAction t (Action r s w a) = [fmt|{rw} | {dir}{to}|]
  where
    (rc, wc) = (boldCol Cyan [r], boldCol Yellow [w])
    rw = style Bold $ rc ++ if r == w then "     " else [fmt| => {wc}|]
    dir = boldCol Green $ if a == "LEFT" then "<-" else "->"
    to = if t == s then "" else [fmt| | {boldCol Red s}|]

-- instance FromJSON Action where
--   parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = stripR '_'})

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

module Data.Model
  ( Action (..),
  -- Program (..),
  )
where

import Data.Aeson
import qualified Data.Map as M
import GHC.Generics
import PyF (fmt)
import System.Console.Pretty (Color (..), Style (..), color, style)
import Util

data Action = Action
  { read_ :: String,
    to_state :: String,
    write :: String,
    action :: String
  }
  deriving (Generic)

instance FromJSON Action where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = stripR '_'}

instance Show Action where
  show = showAction ""

showAction :: String -> Action -> String
showAction t (Action r s w a) = [fmt|{rw} | {dir}{into}|]
  where
    (rc, wc) = (boldCol Cyan r, boldCol Yellow w)
    rw = style Bold $ rc ++ if r == w then "     " else [fmt| => {wc}|]
    dir = boldCol Green $ if a == "LEFT" then "<-" else "->"
    into = if t == s then "" else [fmt| | {boldCol Red s}|]

-- data Program = Program
--   { name :: String,
--     alphabet :: [String],
--     blank :: String,
--     states :: [String],
--     initial :: String,
--     final :: [String],
--     transitions :: M.Map String [Action]
--   }
--   deriving (Generic, Show)

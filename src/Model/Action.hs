module Model.Action (Action (..), pfAction) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)
import PyF (fmt)
import System.Console.Pretty (Color (..), Style (..), color, style)
import Util ( stripR, boldCol )

data Action = Action
  { read_ :: String,
    to_state :: String,
    write :: String,
    action :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Action

instance FromJSON Action where
  parseJSON :: Value -> Parser Action
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = stripR '_'}

pfAction :: String -> Action -> String
pfAction t (Action r s w a) = [fmt|{rw} | {dir}{into}|]
  where
    (rc, wc) = (boldCol Cyan r, boldCol Yellow w)
    rw = style Bold $ rc ++ if r == w then "" else [fmt| => {wc}|]
    dir = boldCol Green $ if a == "LEFT" then "<-" else "->"
    into = if t == s then "" else [fmt| | {boldCol Red s}|]

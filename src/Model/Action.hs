{-# LANGUAGE OverloadedStrings #-}

module Model.Action
  ( Direction (..),
    Action (..),
    pfAction,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import PyF (fmt)
import System.Console.Pretty (Color (..), Style (..), color, style)
import Util (boldCol, stripR)

data Direction = ToLeft | ToRight deriving (Show, Eq)

instance FromJSON Direction where
  parseJSON :: Value -> Parser Direction
  parseJSON = withText "Direction" parseAction

pfDirection :: Direction -> String
pfDirection d = if d == ToLeft then "<-" else "->"

parseAction :: MonadFail f => Text -> f Direction
parseAction t = case T.unpack t of
  "LEFT" -> pure ToLeft
  "RIGHT" -> pure ToRight
  _ -> fail "Invalid direction"

data Action = Action
  { read_ :: Char,
    to_state :: String,
    write :: Char,
    action :: Direction
  }
  deriving (Generic, Show, Eq)

instance FromJSON Action where
  parseJSON :: Value -> Parser Action
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = stripR '_'}

pfAction :: String -> Action -> String
pfAction t (Action r s w a) = [fmt|{rw} | {dir}{into}|]
  where
    (rc, wc) = (boldCol Cyan [r], boldCol Yellow [w])
    rw = style Bold $ rc ++ if r == w then "" else [fmt| => {wc}|]
    dir = boldCol Green $ pfDirection a
    into = if t == s then ("" :: String) else [fmt| | {boldCol Red s}|]

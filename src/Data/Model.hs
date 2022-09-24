module Data.Model
  ( Action (..),
    Program (..),
  )
where

import Data.Aeson
import qualified Data.Map as Map
import Util

data Action = Action
  { read_ :: Char,
    to_state :: String,
    write :: Char,
    action :: String
  }
  deriving (Eq, Show)

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

module Model.Program (Program (..)) where

import Data.Aeson
import Data.Map (Map)
import GHC.Generics (Generic)
import Model.Action (Action)
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

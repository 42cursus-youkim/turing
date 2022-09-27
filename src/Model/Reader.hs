module Model.Reader (readProgram) where

import Data.Aeson (decode, encode, fromJSON)
import qualified Data.ByteString.Lazy as B
import Model.Program (Program)
import Util (note)

data ProgramError = ParseError deriving (Show)

readProgram :: FilePath -> IO (Either ProgramError Program)
readProgram path = do
  note ParseError . decode <$> B.readFile path

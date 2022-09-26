module Model.Reader (testFile, readProgram) where

import Data.Aeson (decode, encode, fromJSON)
import qualified Data.ByteString.Lazy as B
import Model.Program (Program)
import Util (note)

testFile :: FilePath
testFile = "script/unary_sub.json"

-- getJSON :: IO B.ByteString
-- getJSON = B.readFile testFile

data ProgramError = ParseError deriving (Show)

readProgram :: FilePath -> IO (Either ProgramError Program)
readProgram path = do
  note ParseError . decode <$> B.readFile path

-- let machine = Machine program

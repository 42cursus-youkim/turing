import Control.Exception (evaluate)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Data.UnarySub
import Model.Action
import Model.Program (Program (Program))
import Test.Hspec
import Test.QuickCheck (Testable (property))

main :: IO ()
main = hspec do
  describe "Model.Program" do
    it "can load a program" do
      j <- getUnarySubJSON
      let parsed = fromJust (decode j :: Maybe Program)
      parsed `shouldBe` unarySub

{-
  describe "Prelude.head" do
    it "returns the first element of a list" do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $
        \x xs -> head (x : xs) == (x :: Int)

    it "throws an exception if used with an empty list" do
      evaluate (head []) `shouldThrow` anyException
-}

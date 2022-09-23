import Control.Exception (evaluate)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Data.UnarySub
import Model.Action
import Model.Program (Program (Program))
import Test.Hspec
import Test.QuickCheck (Testable (property))
import Util (indent)

main :: IO ()
main = hspec do
  describe "Model.Program" do
    it "can load a program" do
      j <- getUnarySubJSON
      let parsed = fromJust (decode j :: Maybe Program)
      parsed `shouldBe` unarySub

  describe "Util.indent" do
    it "indents a string" do
      indent 2 "foo\n" `shouldBe` "  foo\n"

    it "indents a multiline string" do
      indent 2 "foo\nbar\n" `shouldBe` "  foo\n  bar\n"

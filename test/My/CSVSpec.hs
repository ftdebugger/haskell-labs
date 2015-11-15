module My.CSVSpec where

import My.CSV
import My.Arguments

import Test.Hspec

csv :: [[String]]
csv = [["number", "text"], ["1", "123"]]

emptyCsv :: [[String]]
emptyCsv = [["123", "456"], [""]]

spec :: Spec
spec = do
  describe "CSV" $ do
    it "removes leading header" $ do
      filterHeader startOptions {stripHeader = True} csv `shouldBe` [["1", "123"]]
      filterHeader startOptions {stripHeader = False} csv `shouldBe` [["number", "text"], ["1", "123"]]

    it "removes first column" $ do
      filterFirst startOptions {stripFirst = True} csv `shouldBe` [["text"], ["123"]]
      filterFirst startOptions {stripFirst = False} csv `shouldBe` [["number", "text"], ["1", "123"]]

    it "removes last column" $ do
      filterLast startOptions {stripLast = True} csv `shouldBe` [["number"], ["1"]]
      filterLast  startOptions {stripLast = False} csv `shouldBe` [["number", "text"], ["1", "123"]]

    it "remove empty lines" $ do
      filterEmptyLines emptyCsv `shouldBe` [["123", "456"]]

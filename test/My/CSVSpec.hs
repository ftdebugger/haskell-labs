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
      (filterHeader [Header] csv) `shouldBe` [["1", "123"]]
      (filterHeader [] csv) `shouldBe` [["number", "text"], ["1", "123"]]
    it "removes first column" $ do
      (filterFirst [First] csv) `shouldBe` [["text"], ["123"]]
      (filterFirst [] csv) `shouldBe` [["number", "text"], ["1", "123"]]
      (filterHeader [] csv) `shouldBe` [["number", "text"], ["1", "123"]]
    it "removes last column" $ do
      (filterLast [Last] csv) `shouldBe` [["number"], ["1"]]
      (filterLast [] csv) `shouldBe` [["number", "text"], ["1", "123"]]
      (filterHeader [] csv) `shouldBe` [["number", "text"], ["1", "123"]]
    it "removes last column" $ do
      (filterLast [Last] csv) `shouldBe` [["number"], ["1"]]
      (filterLast [] csv) `shouldBe` [["number", "text"], ["1", "123"]]
    it "remove empty lines" $ do
      (filterEmptyLines emptyCsv) `shouldBe` [["123", "456"]]

module My.BayesSpec where

import My.Bayes

import Test.Hspec

spec :: Spec
spec = describe "Bayes" $ do
    it "extract classes" $ do
      let input = [([1, 2], "3"), ([4, 5], "6")]
      let expect = ["3", "6"]

      getBayesClasses input `shouldBe` expect

    it "apriory distribution" $ do
      let input = [([1, 2], "1"), ([4, 5], "2"), ([3, 4], "1"), ([2, 7], "2")]

      aprioryBayesDistribution input ["1", "2"] `shouldBe` [("1", 0.5, [2, 3], [1, 1]), ("2", 0.5, [3, 6], [17, 5])]

    it "probability of object belongs class" $ do
      let classDistr = ("1", 0.5, [2, 3], [0, 0])
      let object = ([1, 1], "1")
      let result = probabilityOfObjectBelongsClass object classDistr

      head result - 0.16 < 0.01 `shouldBe` True
      last result - 0.11 < 0.01 `shouldBe` True

    it "compute object class" $ do
      let classDistr = [("1", 0.5, [2, 3], []), ("2", 0.5, [3, 6], [])]

      getProbableObjectClass ([1, 2], "1") classDistr `shouldBe` "1"
      getProbableObjectClass ([4, 5], "2") classDistr `shouldBe` "2"
      getProbableObjectClass ([3, 4], "1") classDistr `shouldBe` "1"
      getProbableObjectClass ([2, 7], "2") classDistr `shouldBe` "2"

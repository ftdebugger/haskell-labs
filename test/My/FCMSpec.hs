module My.FCMSpec where

import My.FCM
import qualified Data.Vector as V

import Test.Hspec

spec :: Spec
spec = do
  describe "FCM Distance" $ do
    it "calculate euclid distance" $ do
      let v1 = V.fromList [1, 2, 3]
      let v2 = V.fromList [1, 5, 7]

      euclidDistance v1 v2 `shouldBe` 5

    it "calculate hamming distance" $ do
      let v1 = V.fromList [1, 2, 3]
      let v2 = V.fromList [1, 5, 7]

      hammingDistance v1 v2 `shouldBe` 7

  describe "FCM matrix" $ do
    it "generate random line" $ do
      line <- generateLine 3
      sum line `shouldBe` 1
    it "generate random matrix" $ do
      matrix <- generateAccessory 3 2
      (sum $ map sum matrix) `shouldBe` 2

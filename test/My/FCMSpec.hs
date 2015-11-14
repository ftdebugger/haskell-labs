module My.FCMSpec where

import My.FCM

import Test.Hspec

spec :: Spec
spec = do
  describe "FCM Distance" $ do
    it "calculate euclid distance" $ do
      let v1 = [1, 2, 3]
      let v2 = [1, 5, 7]

      euclidDistance v1 v2 `shouldBe` 5

    it "calculate hamming distance" $ do
      let v1 = [1, 2, 3]
      let v2 = [1, 5, 7]

      hammingDistance v1 v2 `shouldBe` 7

  describe "FCM matrix" $ do
    it "generate random line" $ do
      line <- generateLine 3
      sum line `shouldBe` 1
    it "generate random matrix" $ do
      matrix <- generateAccessory 3 2
      sum (map sum matrix) `shouldBe` 2

  describe "Utils" $ do
    it "generate zero vector" $
      zeroVector 5 `shouldBe` [0, 0, 0, 0, 0]

    it "calculate accessory distance with euclid" $ do
      let v1 = [1, 2, 3]
      let v2 = [1, 5, 7]
      let dist = accessoryDistance euclidDistance v1 v2 0.1

      getRandomCenters 2 [[1], [2], [3]] >>= print

      abs ( dist - 0.05 ) < 0.0001 `shouldBe` True

    it "calculate accessory distance with hamming" $ do
      let v1 = [1, 2, 3]
      let v2 = [1, 5, 7]
      let dist = accessoryDistance hammingDistance v1 v2 0.1

      abs ( dist - 0.07 ) < 0.0001 `shouldBe` True

    it "calculate accessories distance with euclid" $ do
      let v1 = [1, 2, 3]
      let v2 = [[1, 5, 7], [1, 5, 7]]
      let acc = [0.1, 0.5]

      let dist = accessoriesDistance euclidDistance v1 v2 acc

      dist `shouldBe` 1.3

  describe "Clusterize" $
    it "should create correct clusters" $ do
      let objects = [[1, 2, 3], [1, 3, 4],[1, 1, 5],[1, 5, 6]]
      let centers = [[1, 2, 3], [1, 3, 4]]

      let clusters = cluserize euclidDistance centers objects
      print clusters
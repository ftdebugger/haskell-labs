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

    it "fcmSelectCenters" $ do
      let objects = [[1, 2, 3], [1, 3, 4], [1, 1, 5], [1, 5, 6]]
      let accessories = [[1, 1, 0, 0], [0, 0, 1, 1]]

      let result = fcmSelectCenters accessories objects
      let expect = [[1.0,2.5,3.5],[1.0,3.0,5.5]]

      result `shouldBe` expect
    it "calcAccessoryForObj" $ do
      let objects = [[1, 2, 3], [1, 3, 4], [1, 1, 5], [1, 5, 6]]
      let centers = [[1.0, 2.5, 3.5], [1.0, 3.0, 5.5]]

      let result = fcmCalculateAccessories euclidDistance centers objects
      let sums = zipWith (+) (head result) (result !! 1)

      sums `shouldBe` [1, 1, 1, 1]

    it "fcmMatrixNorm" $ do
      let a1 = [[1, 1, 0, 0], [0, 0, 1, 1]]
      let a2 = [[1, 1, 0.5, 0], [0, 0, 0.5, 1]]

      let result = fcmMatrixNorm a1 a2
      result `shouldBe` 0.5

    it "fcm process" $ do
      let objects = [[16, 2, 3], [2, 3, 4], [1, 1, 5], [2, 5, 6]]

      result <- fcmProcess euclidDistance 2 0.01 objects

      let sums = zipWith (+) (head result) (result !! 1)

      sums `shouldBe` [1, 1, 1, 1]
    -- it "calculate  accessory distance with euclid" $ do
    --   let v1 = [1, 2, 3]
    --   let v2 = [1, 5, 7]
    --   let dist = accessoryDistance euclidDistance v1 v2 0.1
    --
    --   abs ( dist - 0.05 ) < 0.0001 `shouldBe` True
    --
    -- it "calculate accessory distance with hamming" $ do
    --   let v1 = [1, 2, 3]
    --   let v2 = [1, 5, 7]
    --   let dist = accessoryDistance hammingDistance v1 v2 0.1
    --
    --   abs ( dist - 0.07 ) < 0.0001 `shouldBe` True
    --
    -- it "calculate accessories distance with euclid" $ do
    --   let v1 = [1, 2, 3]
    --   let v2 = [[1, 5, 7], [1, 5, 7]]
    --   let acc = [0.1, 0.5]
    --
    --   let dist = accessoriesDistance euclidDistance v1 v2 acc
    --
    --   dist `shouldBe` 1.3

  describe "Clusterize" $ do
    it "should create correct clusters" $ do
      let objects = [[1, 2, 3], [1, 3, 4],[1, 1, 5],[1, 5, 6]]
      let centers = [[1, 2, 3], [1, 3, 4]]

      let clusters = clusterize euclidDistance centers objects
      let expect = [([1.0,2.0,3.0],[[1.0,2.0,3.0],[1.0,1.0,5.0]]),([1.0,3.0,4.0],[[1.0,3.0,4.0],[1.0,5.0,6.0]])]

      clusters `shouldBe` expect

    it "should select cluster centers" $ do
      let objects = [[1, 2, 3], [1, 2, 3], [1, 3, 4], [1, 1, 5], [1, 5, 6]]
      let centers = [[1, 1, 5], [1, 5, 6]]

      let clusters = clusterize euclidDistance centers objects
      let newCenters = selectCenters euclidDistance clusters

      let expect = [[1, 2, 3], [1, 5, 6]]

      newCenters `shouldBe` expect

    it "should return result clusters" $ do
      let objects = [[1, 2, 3], [1, 2, 3], [1, 3, 4], [1, 1, 5], [1, 5, 6]]

      clusters <- fcm euclidDistance 2 objects
      length clusters `shouldBe` 2

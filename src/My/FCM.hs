module My.FCM where

import System.Random
import Control.Monad
import Data.List

type Points = [Double]
type Matrix = [Points]

type Object = [Double]
type Objects = [Object]

type Accesorry = Double
type Accessories = [Accesorry]

type Center = Object
type Centers = [Center]

type Cluster = (Center, Objects)
type Clusters = [Cluster]

type Distance = Object -> Object -> Double

euclidDistance :: Distance
euclidDistance v1 v2 = sqrt $ sum $ zipWith diff v1 v2
  where diff a b = (a-b)^(2::Int)

hammingDistance :: Distance
hammingDistance v1 v2 = sum $ zipWith diff v1 v2
  where diff a b = abs $ a-b

generateLine :: Int -> IO Points
generateLine n = do
  g <- getStdGen
  let line = take n (randomRs (0, 1) g)
  let lineSum = sum line

  return $ map (/ lineSum) line

generateAccessory :: Int -> -- N
                     Int -> -- C
                     IO Matrix
generateAccessory n c = replicateM c (generateLine n)

zeroVector :: Int -> [Double]
zeroVector n = replicate n 0

accessoryDistance :: Distance ->
                  Center ->
                  Object -> -- accessory
                  Accesorry ->
                  Double
accessoryDistance distance center obj accessory = accessory * accessory * dist
  where dist = distance center obj

accessoriesDistance :: Distance ->
                      Center ->
                      Objects ->
                      Accessories ->
                      Double
accessoriesDistance distance center objects accessories = sum $ map dist zipDist
  where zipDist = zip objects accessories
        dist (obj, accessory) = accessoryDistance distance center obj accessory

randInRange :: Int -> Int -> IO Int
randInRange a b = getStdRandom $ randomR (a, b)

randomPick :: [a] -> IO a
randomPick xs = liftM (xs !!) (randomRIO (0, length xs - 1))

-- getRandomCenters :: Int -> -- Clucsters
--                     Objects ->
--                     IO Centers
getRandomCenters :: Int -> Objects -> IO Objects
getRandomCenters c objects = replicateM c (pick c)
  where pick _ = randomPick objects

cluserize :: Distance -> Centers -> Objects -> [Cluster]
cluserize distance centers objects = createClusters getCenters
  where createClusters objs = map (createCluster objs) centers
        createCluster objs center = (center, pickWithCenter center objs)
        pickWithCenter center objs = map first $ filter (filterWithCenter center) objs
        filterWithCenter center (_, a) = a == center

        getCenters = map getCenter objects
        getCenter object = (object, first $ minCenter $ distances object)
        distances object = map (dist object) centers
        dist object center = (center, distance object center)
        minCenter = minimumBy cmp
        cmp a b = compare (second a) (second b)
        second (_, numDist) = numDist
        first (center, _) = center

-- calculateCenters :: Int -> -- number of clusters
--                     Matrix -> -- input matrix
--                     Matrix -> -- accessory matrix
--                     Points -- cetners

-- fcm :: Distance -> -- Distnace function (euclid or hamming)
--        Int -> -- number of clusters
--        Float -> -- accuracy
--        Matrix -> -- accessory matrix
--        Matrix -> -- matrix with N number vectors
--        Matrix --  result matrix with acces
-- fcm distance clustersCount eps accessory matrix = [[]]

module My.FCM where

import System.Random
import Control.Monad
import Data.List
import Data.List.Split

type Points = [Double]
type Matrix = [Points]

type Object = [Double]
type Objects = [Object]

type Accesorry = [Double]
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
generateAccessory n c = do
  g <- getStdGen

  return $ createMatrix (getNumbers g)
  where getNumbers g = take (n * c) (randomRs (0, 1) g :: [Double])
        createMatrix nums = transpose $ map normalize $ chunksOf n nums
        normalize row = map (/ sum row) row

getRandomCenters :: Int -> Objects -> IO Objects
getRandomCenters c objects = do
  g <- getStdGen

  return $ pick (randomSeq g)

  where randomSeq g = take c (randomRs (0, length objects - 1) g :: [Int])
        pick = map (\n -> objects !! n)

zeroVector :: Int -> [Double]
zeroVector n = replicate n 0

fcmSelectCenters :: Accessories ->
                    Objects ->
                    Centers
fcmSelectCenters accessories objects = map fcmSelectCenter accessories
  where fcmSelectCenter accessory = map (/ accSum) elemSum
          where elemSum = foldl (zipWith (+)) zero (zipWith mult accessory objects)
                 where mult m = map (\o -> m ** 2 * o)
                accSum = sum $ map (**2) accessory
                zero = replicate (length (head objects)) 0

fcmCalculateAccessories :: Distance ->
                           Centers ->
                           Objects ->
                           Accessories
fcmCalculateAccessories distance centers objects = map calcAccessory centers
  where calcAccessory center = map (calcAccessoryForObj center) objects
        calcAccessoryForObj center object = 1 / sum'
          where sum' = sum $ map rel centers
                rel nCenter = (distance object center / distance object nCenter) ** 2

fcmMatrixNorm :: Accessories -> Accessories -> Double
fcmMatrixNorm a b = maximum $ zipWith reduceLines a b
  where reduceLines c d = maximum $ map abs (zipWith (-) c d)

fcmProcess :: Distance ->
              Int -> -- c
              Double ->
              Bool ->
              Objects ->
              IO Accessories
fcmProcess distance c eps randomCenters objects = do
  accessories <- if randomCenters
                  then do
                    centers <- getRandomCenters c objects
                    return $ fcmCalculateAccessories distance centers objects
                  else generateAccessory c (length objects)

  return $ fcm' accessories

  where fcm' accessories = calcNewAccessory (fcmSelectCenters accessories objects)
          where
            calcNewAccessory centers = checkAccessories (fcmCalculateAccessories distance centers objects)
            checkAccessories newAccessories
              | fcmMatrixNorm accessories newAccessories < eps = newAccessories
              | otherwise = fcm' newAccessories

-- From Wikipedia


randInRange :: Int -> Int -> IO Int
randInRange a b = getStdRandom $ randomR (a, b)

randomPick :: [a] -> IO a
randomPick xs = liftM (xs !!) (randomRIO (0, length xs - 1))

clusterize :: Distance -> Centers -> Objects -> Clusters
clusterize distance centers objects = createClusters getCenters
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

selectCenters :: Distance -> Clusters -> Centers
selectCenters distance = map selectCenter
  where selectCenter (_, objects) = first $ minCenter objects
        first (a, _) = a
        minCenter objects = minimumBy compareMins $ map objectDistance objects
          where objectDistance object = (object, sum $ map (distance object) objects)
                compareMins (_, a) (_, b) = compare a b

fcm :: Distance ->
       Int -> -- cluster count
       Objects ->
       IO Clusters

fcm distance c objects = do
  centers <- getRandomCenters c objects
  return $ fcm' centers
  where fcm' centers = checkCluster (clusterize distance centers objects)
          where checkCluster cluster = compareCenters cluster (selectCenters distance cluster)
                compareCenters cluster newCenters
                  | centers == newCenters = cluster
                  | otherwise = fcm' newCenters

-- fcm :: Distance -> -- Distnace function (euclid or hamming)
--        Int -> -- number of clusters
--        Float -> -- accuracy
--        Matrix -> -- accessory matrix
--        Matrix -> -- matrix with N number vectors
--        Matrix --  result matrix with acces
-- fcm distance clustersCount eps accessory matrix = [[]]

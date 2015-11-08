module My.FCM where

import qualified Data.Vector as V
import System.Random

type Distance = V.Vector Double -> V.Vector Double -> Double
type Points = [Float]
type Matrix = [Points]

euclidDistance :: Distance
euclidDistance v1 v2 = sqrt $ V.sum $ V.zipWith diff v1 v2
  where diff a b = (a-b)^(2::Int)

hammingDistance :: Distance
hammingDistance v1 v2 = V.sum $ V.zipWith diff v1 v2
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
generateAccessory n c = mapM generateLine (replicate c n)

-- generateAccessory n 1 = do
--   line <- generateLine n
--   return [line]
-- generateAccessory n c = do
--   line <- generateLine n
--   matrix <- generateAccessory n (c - 1)
--
--   return line:matrix :: IO Matrix

fcm :: Distance -> -- Distnace function (euclid or hamming)
       Int -> -- number of clusters
       Float -> -- accuracy
       Matrix -> -- accessory matrix
       Matrix -> -- matrix with N number vecotrs
       Matrix --  result matrix with acces
fcm distance clustersCount eps accessory matrix = [[]]

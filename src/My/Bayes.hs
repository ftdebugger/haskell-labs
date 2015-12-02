module My.Bayes where

import System.Random
import Data.List

type BayesAttributes = [Double]
type BayesClass = Double
type BayesClasses = [Double]
type BayesObject = (BayesAttributes, BayesClass)
type BayesObjects = [BayesObject]
type BayesInput = (BayesObjects, BayesObjects)

type BayesClassDistribution = (BayesClass, Double, [Double], [Double]) -- (C, P(C), X_lc, sigma)
type BayesClassDistributions = [BayesClassDistribution]

type BayesProbability = [Double]
type BayesProbabilities = [BayesProbability]

listToBayesObjects :: [[Double]] -> BayesObjects
listToBayesObjects = map conv
  where conv xs = (init xs, last xs)


prepareBayesInput :: [[Double]] -> Double -> [Double] -> IO BayesInput
prepareBayesInput input rnd rndSeq = do
  let distributed = zip rndSeq input
  let trainingSet = map second $ filter (\(a, _) -> a < rnd) distributed
  let checkSet = map second $ filter (\(a, _) -> a >= rnd) distributed

  return (listToBayesObjects trainingSet, listToBayesObjects checkSet)

  where second (_, xs) = xs

getBayesClasses :: BayesObjects -> BayesClasses
getBayesClasses objects = nub $ map getClass objects
  where getClass (_, c) = c

aprioryBayesDistribution :: BayesObjects -> BayesClasses -> BayesClassDistributions
aprioryBayesDistribution objects = map distr
  where distr cl = (cl, calc cl, aver cl, sigma cl (aver cl))
        calc cl = fromIntegral (length $ objectsOfClass cl) / fromIntegral (length objects)
        objectsOfClass cl = filter (ofClass cl) objects
        ofClass c1 (_, c2) = c1 == c2
        extractAttributes (xs, _) = xs
        aver cl = map avg (transpose $ map extractAttributes (objectsOfClass cl))
          where avg xs = sum xs / fromIntegral (length xs)
        sigma cl x_lc = map sigma' (transpose $ map extractAttributes (objectsOfClass cl))
          where sigma' attrs = sum (map disp (zip attrs x_lc)) / fromIntegral (length attrs - 1)
                disp (attr, x_lc_i) = (attr - x_lc_i) ** 2

probabilityOfObjectBelongsClass :: BayesObject -> BayesClassDistribution -> BayesProbability
probabilityOfObjectBelongsClass (attrs, _) (_, _, x_lc, _) = map calc pairs
  where pairs = zip attrs x_lc
        sigma = sum (map disp pairs) / fromIntegral (length attrs - 1)
          where disp (attr, x_lc_i) = (attr - x_lc_i) ** 2
        calc (attr, x_lc_i) = exp power / denominator
          where power = (attr - x_lc_i) ** 2 / ( -2 * sigma )
                denominator = sqrt (2 * pi * sigma)

getProbableObjectClass :: BayesObject -> BayesClassDistributions -> BayesClass
getProbableObjectClass object classes = argmax $ map probability classes
  where argmax probablities = extractClass $ maximumBy mx probablities
        probability (c, p, xs, s) = (c, p * mult (c, p, xs, s))
        mult clDistr = product (probabilityOfObjectBelongsClass object clDistr)
        mx (_, a) (_, b) = compare a b
        extractClass (c, _) = c

bayesProcess :: [[Double]] -> Double -> Int -> IO (BayesClassDistributions)
bayesProcess input rnd count = do
  g <- getStdGen
  (_, distribution) <- step (randoms g :: [Double]) count (1, [])

  return distribution

  where step rndSeq cnt best
          | cnt == 0 = return best
          | otherwise = do
              current <- process rndSeq
              step (drop (length input) rndSeq) (cnt - 1) (mx best current)

        mx (aError, aDist) (bError, bDist)
          | aError < bError = (aError, aDist)
          | otherwise = (bError, bDist)

        process rndSeq = do
                (trainingSet, checkSet) <- prepareBayesInput input rnd rndSeq

                let distribution = aprioryBayesDistribution trainingSet $ getBayesClasses trainingSet

                let checkResult = map check checkSet
                                    where check object = comp object (getProbableObjectClass object distribution)
                                          comp (_, c1) c2 = c1 == c2

                let failed = length (filter not checkResult)
                let errorRate = fromIntegral failed / fromIntegral (length checkResult)
                let result = (errorRate, distribution)

                return result

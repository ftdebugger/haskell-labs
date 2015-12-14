module My.Bayes where

import System.Random
import Data.List
import Data.List.Split
import Control.Parallel.Strategies

type BayesAttributes = [Double]
type BayesClass = String
type BayesClasses = [BayesClass]
type BayesObject = (BayesAttributes, BayesClass)
type BayesObjects = [BayesObject]
type BayesInput = (BayesObjects, BayesObjects)

type BayesClassDistribution = (BayesClass, Double, [Double], [Double]) -- (C, P(C), X_lc, sigma)
type BayesClassDistributions = [BayesClassDistribution]

type BayesProbability = [Double]
type BayesProbabilities = [BayesProbability]

prepareBayesInput :: BayesObjects -> Double -> [Double] -> BayesInput
prepareBayesInput input rnd rndSeq = (trainingSet, checkSet)

  where second (_, xs) = xs
        distributed = zip rndSeq input
        trainingSet = map second $ filter (\(a, _) -> a < rnd) distributed
        checkSet = map second $ filter (\(a, _) -> a >= rnd) distributed

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

bayesProcess :: BayesObjects -> Double -> Int -> IO BayesClassDistributions
bayesProcess input rnd count = do
  g <- getStdGen

  return $ bestResult (runEval $ threadMap makeTry (randomChunks g))

  where
        first (a, _) = a
        second (_, b) = b
        makeTry :: [Double] -> (Double, BayesClassDistributions)
        makeTry rndSeq = (errorRate, distribution)
          where
            makeSets = prepareBayesInput input rnd rndSeq
            trainingSet = first makeSets
            checkSet = second makeSets
            distribution = aprioryBayesDistribution trainingSet $ getBayesClasses trainingSet

            checkResult = map check checkSet
                                where check object = comp object (getProbableObjectClass object distribution)
                                      comp (_, c1) c2 = c1 == c2
            failed = length (filter not checkResult)
            errorRate = fromIntegral failed / fromIntegral (length checkResult)

        bestResult results = second $ minimumBy comp results
          where comp (a, _) (b, _) = compare a b

        randomChunks g = take count $ chunksOf (length input) (randoms g :: [Double])

        threadMap :: (a -> b) -> [a] -> Eval [b]
        threadMap _ [] = return []
        threadMap f (a:as) = do
           b <- rpar (f a)
           bs <- threadMap f as
           return (b:bs)

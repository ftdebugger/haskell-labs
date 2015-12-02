module My.Arguments(parseArguments, Options(..), startOptions) where

-- import System.Exit

-- import Data.List
import System.Console.GetOpt
-- import System.IO

import My.FCM

data Options = Options { stripHeader :: Bool
                       , stripFirst :: Bool
                       , stripLast :: Bool
                       , distance :: Distance
                       , clustersCount :: Int
                       , randomCenters :: Bool
                       , help :: Bool
                       , input :: String
                       , output :: String
                       , eps :: Double
                       , steps :: Int
                       , trainSize :: Double
                       }

startOptions :: Options
startOptions = Options { stripHeader = False
                       , stripFirst = False
                       , stripLast = False
                       , distance = euclidDistance
                       , clustersCount = 2
                       , randomCenters = False
                       , help = False
                       , input = ""
                       , output = "out.txt"
                       , eps = 0.01
                       , steps = 50
                       , trainSize = 0.8
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
   [
       Option "h" ["header"] (NoArg (\opt -> return opt {stripHeader = True})) "Ignore header",
       Option "f" ["first"] (NoArg (\opt -> return opt {stripFirst = True})) "Ignore first column",
       Option "l" ["last"] (NoArg (\opt -> return opt {stripLast = True})) "Ignore last column",
       Option "e" ["euclid"] (NoArg (\opt -> return opt {distance = euclidDistance})) "Euclid distance",
       Option "m" ["hamming"] (NoArg (\opt -> return opt {distance = hammingDistance})) "Hamming distance",
       Option "c" ["cluster"] (ReqArg (\arg opt -> return opt {clustersCount = read arg :: Int}) "NUMBER") "Cluster count",
       Option "r" ["random"] (NoArg (\opt -> return opt {randomCenters = True})) "Random centers",
       Option "p" ["eps"] (ReqArg (\arg opt -> return opt {eps = read arg :: Double}) "DOUBLE") "EPS",
       Option "x" ["help"] (NoArg (\opt -> return opt {help = True})) "Help",
       Option "i" ["input"] (ReqArg (\arg opt -> return opt {input = arg}) "STRING") "Input file",
       Option "o" ["output"] (ReqArg (\arg opt -> return opt {output = arg}) "STRING") "Output file",
       Option "t" ["steps"] (ReqArg (\arg opt -> return opt {steps = read arg :: Int}) "NUMBER") "Count of steps",
       Option "s" ["trainSize"] (ReqArg (\arg opt -> return opt {trainSize = read arg :: Double}) "NUMBER") "Train size [0..1]"
   ]

parseArguments :: [String] -> IO Options
parseArguments args = do
  let (actions, _, _) = getOpt RequireOrder options args
  foldl (>>=) (return startOptions) actions

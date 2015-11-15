import System.Environment

import My.Arguments
import My.CSV
import My.FCM
import Data.List

-- Main

main :: IO [()]
main = do
    (flags, files) <- getArgs >>= parseArguments
    print files
    print flags
    csv <- mapM (parseFile flags) files

    mapM (processCSV flags) csv

    where processCSV flags csv = do
            accessories <- process (csvToObjects csv)

            printAccessories $ transpose accessories

            return ()

            where
              csvToObjects = map (map conv)
                where conv v = read v :: Double
              process objects = fcmProcess selectDistance 5 0.01 objects
                where selectDistance
                        | Euclid `elem` flags = euclidDistance
                        | Hemming `elem` flags = hammingDistance
                        | otherwise = euclidDistance
                      -- selectClusterCount
                      --   | ClustersCount `elem` flags = flags ClustersCount
                      --   | otherwise = 2
              printAccessories = mapM printAccessory
                where printAccessory accessory = do
                        print accessory

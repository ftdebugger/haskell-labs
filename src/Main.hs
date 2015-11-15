import System.Environment

import My.Arguments
import My.CSV
import My.FCM

-- Main

main :: IO [()]
main = do
    (flags, files) <- getArgs >>= parseArguments
    print files
    print flags
    csv <- mapM (parseFile flags) files

    mapM (processCSV flags) csv

    where processCSV flags csv = do
            clusters <- process (csvToObjects csv)

            printClusters clusters

            return ()

            where
              csvToObjects = map (map conv)
                where conv v = read v :: Double
              process objects = fcm selectDistance 2 objects
                where selectDistance
                        | Euclid `elem` flags = euclidDistance
                        | Hemming `elem` flags = hammingDistance
                        | otherwise = euclidDistance
                      -- selectClusterCount
                      --   | ClustersCount `elem` flags = flags ClustersCount
                      --   | otherwise = 2
              printClusters = mapM printCluster
                where printCluster (center, objects) = do
                        print $ "Cluster " ++ show center
                        print objects

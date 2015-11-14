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

    mapM processCSV csv

    where processCSV csv = do
            clusters <- process (csvToObjects csv)

            printClusters clusters

            return ()

            where
              csvToObjects = map (map conv)
                where conv v = read v :: Double
              process objects = fcm euclidDistance 2 objects
              printClusters = mapM printCluster
                where printCluster (center, objects) = do
                        print $ "Cluster " ++ show center
                        print objects

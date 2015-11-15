import System.Environment

import My.Arguments
import My.CSV
import My.FCM
import Data.List

-- Main

main :: IO () --[()]
main = do
    options <- getArgs >>= parseArguments

    let Options {input = file} = options

    csv <- parseFile options file

    processCSV options csv

    where processCSV Options {distance = dist, clustersCount = count, randomCenters = random, eps = _eps} csv = do
            accessories <- process (csvToObjects csv)

            printAccessories $ transpose accessories

            return ()

            where
              csvToObjects = map (map conv)
                where conv v = read v :: Double
              process = fcmProcess dist count _eps random
              printAccessories = mapM printAccessory
                where printAccessory = print

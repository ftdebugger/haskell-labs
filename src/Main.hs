import System.Environment

import My.Arguments
import My.CSV

-- Main

main :: IO [()]
main = do
    (flags, files) <- getArgs >>= parseArguments
    print files
    print flags
    mapM (parseFile flags) files

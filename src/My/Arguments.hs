module My.Arguments where

import System.Exit

import Data.List
import System.Console.GetOpt
import System.IO

data Flag
    = Header
    | First
    | Last
    | Help
    | Euclid
    | Hemming
    | ClustersCount String
    deriving (Eq, Ord, Show)

defaultFlags :: [OptDescr Flag]
defaultFlags =
    [
        Option "h" ["header"] (NoArg Header) "Ignore header",
        Option "f" ["first"] (NoArg First) "Ignore first column",
        Option "l" ["last"] (NoArg Last) "Ignore last column",
        Option "e" ["euclid"] (NoArg Euclid) "Euclid distance",
        Option "m" ["hemming"] (NoArg Hemming) "Euclid distance",
        Option "c" ["cluster"] (ReqArg ClustersCount "number") "Cluster count",
        Option "x" ["help"] (NoArg Help) "Help"
    ]

parseArguments :: [String] -> IO ([Flag], [String])
parseArguments argv = case getOpt Permute defaultFlags argv of
    (args, fs, []) -> do
        let files = if null fs then [] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header defaultFlags)
                    exitSuccess
            else return (nub args, files)

    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header defaultFlags)
        exitWith (ExitFailure 1)

    where header = "Usage: lab [-hfl] [file ...]"

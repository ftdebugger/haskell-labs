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
    deriving (Eq, Ord, Enum, Show, Bounded)

defaultFlags :: [OptDescr Flag]
defaultFlags =
    [
        Option "h" ["header"] (NoArg Header) "Ignore header",
        Option "f" ["first"] (NoArg First) "Ignore first column",
        Option "l" ["last"] (NoArg Last) "Ignore last column"
    ]

parseArguments :: [String] -> IO ([Flag], [String])
parseArguments argv = case getOpt Permute defaultFlags argv of
    (args, fs, []) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header defaultFlags)
                    exitSuccess
            else return (nub args, files)

    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header defaultFlags)
        exitWith (ExitFailure 1)

    where header = "Usage: cat [-benstuv] [file ...]"

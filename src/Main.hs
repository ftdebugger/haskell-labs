import System.Environment

import My.Arguments
import My.CSV
import My.Bayes
import Pipes
import Data.List
import System.IO

import qualified Pipes.Prelude as P

-- Main

-- main :: IO ()
main = do
    options <- getArgs >>= parseArguments

    let Options {input = file} = options

    let lineStream = parseFileWithPipes options file
    let splitedStream = lineStream >-> collect []
    lastStream <- P.last splitedStream

    process lastStream

    return ()
    -- runEffect $ for lastStream (lift . print)

    where collect xs = do
            line <- await
            yield xs
            collect $ xs ++ [line]

          process m = case m of
            Just v -> do
              bayesInput <- bayesProcess v 0.8 50

              runEffect $ each (outputResult bayesInput) >-> pipePrint

          outputResult output = map outputLine output
          outputLine (c, p, mat, sigma) = (show c) ++ " - " ++ (outputSigma (zip mat sigma))
          outputSigma params = intercalate " " $ map out (zip params [1..])
            where out ((m, s), index) = (show index) ++ "(" ++ (show m) ++ "; " ++ (show s) ++ ")"

          pipePrint = do
            h <- lift $ openFile "out.txt" WriteMode
            P.toHandle h

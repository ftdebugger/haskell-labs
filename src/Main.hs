import System.Environment

import My.Arguments
import My.CSV
import My.Bayes
import Pipes
import System.IO

import qualified Pipes.Prelude as P

-- Main

main :: IO ()
main = do
    options <- getArgs >>= parseArguments

    let Options {input = file} = options

    let lineStream = parseFileWithPipes options file
    let splitedStream = lineStream >-> collect []
    lastStream <- P.last splitedStream

    process options lastStream

    return ()

    where collect xs = do
            line <- await
            yield xs
            collect $ xs ++ [line]

          process options m = case m of
            Just v -> do
              let Options {output = out, steps = st, trainSize = ts} = options

              bayesInput <- bayesProcess v ts st
              let strs = outputResult bayesInput

              withFile out WriteMode $ \h -> runEffect $ each strs >-> P.toHandle h

          outputResult = map outputLine
          outputLine (c, _, mat, sigma) = show c ++ " - " ++ outputSigma (zip mat sigma)
          outputSigma params = unwords $ zipWith (curry out) params ([1 ..] :: [Integer])
            where out ((m, s), index) =  show index ++ "(" ++ show m ++ "; " ++ show s ++ ")"

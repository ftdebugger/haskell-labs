{-# LANGUAGE ScopedTypeVariables #-}

module My.CSV where

import Data.Char (isSpace)
import Data.List.Split
import Text.CSV
import Pipes
import qualified Pipes.Prelude as P
import System.IO

import My.Arguments

-- parse CSV and normalize

parseFile :: Options -> FilePath -> IO CSV
parseFile options file = do
    csvData <- parseCSVFromFile file
    case csvData of
        Left err -> do
            print err

            return []
        Right contents -> do
            let fd = filterEmptyLines contents
            let dt = filterLast options (filterFirst options (filterHeader options fd))

            return dt

parseFileWithPipes :: Options -> FilePath -> Producer [Double] IO ()
parseFileWithPipes options file = do
  h <- lift $ openFile file ReadMode
  -- P.toListM $
  -- P.fromHandle h >-> parseLine
  -- let p = P.fromHandle h >-> parseLine
  -- return $ P.toListM (P.fromHandle h >-> parseLine)
  P.fromHandle h >-> parseLine
  -- return $ (lift (P.toList p) >>= yield)
  -- return $ P.fromHandle h >-> parseLine >-> collect []

  where parseLine = do
          str <- await
          yield $ parseLine' str
          parseLine
        parseLine' s = map conv $ splitOn "," $ trim s
        conv v = read v :: Double
        trim = f . f
          where f = reverse . dropWhile isSpace

filterHeader :: Options -> CSV -> CSV
filterHeader Options {stripHeader = s} csvData
    | s = tail csvData
    | otherwise = csvData

filterFirst :: Options -> CSV -> CSV
filterFirst Options {stripFirst = s} csvData
    | s = map tail csvData
    | otherwise = csvData

filterLast :: Options -> CSV -> CSV
filterLast Options {stripLast = s} csvData
    | s = map init csvData
    | otherwise = csvData

filterEmptyLines :: CSV -> CSV
filterEmptyLines = filter (\x -> x/=[""])

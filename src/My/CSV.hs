module My.CSV where

import Text.CSV

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

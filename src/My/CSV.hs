module My.CSV where

import Text.CSV

import My.Arguments

-- parse CSV and normalize

parseFile :: [Flag] -> FilePath -> IO CSV
parseFile flags file = do
    csvData <- parseCSVFromFile file
    case csvData of
        Left err -> do
            print err

            return []
        Right contents -> do
            let fd = filterEmptyLines contents
            let dt = filterLast flags (filterFirst flags (filterHeader flags fd))

            return dt

filterHeader :: [Flag] -> CSV -> CSV
filterHeader flags csvData
    | Header `elem` flags = tail csvData
    | otherwise = csvData

filterFirst :: [Flag] -> CSV -> CSV
filterFirst flags csvData
    | First `elem` flags = map tail csvData
    | otherwise = csvData

filterLast :: [Flag] -> CSV -> CSV
filterLast flags csvData
    | Last `elem` flags = map init csvData
    | otherwise = csvData

filterEmptyLines :: CSV -> CSV
filterEmptyLines = filter (\x -> x/=[""])

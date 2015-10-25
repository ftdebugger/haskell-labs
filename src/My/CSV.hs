module My.CSV where

import Text.CSV

import My.Arguments

-- parse CSV and normalize

parseFile :: [Flag] -> FilePath -> IO ()
parseFile flags file = do
    csvData <- parseCSVFromFile file
    case csvData of
        Left err -> print err
        Right contents -> do
            let fd = filter(\x -> x/=[""]) contents
            let dt = filterLast flags (filterFirst flags (filterHeader flags fd))
            let cd = map (map conv) dt where conv v = read v :: Float

            print cd
            return ()

filterHeader :: [Flag] -> CSV -> [Record]
filterHeader flags csvData
    | Header `elem` flags = tail csvData
    | otherwise = csvData

filterFirst :: [Flag] -> CSV -> [Record]
filterFirst flags csvData
    | First `elem` flags = map tail csvData
    | otherwise = csvData

filterLast :: [Flag] -> CSV -> [Record]
filterLast flags csvData
    | Last `elem` flags = map init csvData
    | otherwise = csvData

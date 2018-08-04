module Csv
(
    getDataFromCsv
)
where

import DesisionTree
import System.IO (readFile)
import Data.List.Split (splitOn)

getDataFromCsv :: String -> Int -> IO ([[Feature]], Int)
getDataFromCsv filename outFeature = do
    contents <- readFile filename
    let features = parseCsv contents
    return (features, outFeature)

parseCsv :: String -> [[Feature]]
parseCsv contents = 
    map (map (read :: String -> Feature)) $ map (splitOn ",") $ lines contents 



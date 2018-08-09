module Main where

import Csv
import RandomForest

main :: IO
main = do
    input <- getDataFromCsv "iris.csv" 0
    forest <- train input 25 3    
    print $ predict forest []

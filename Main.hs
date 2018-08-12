module Main where

import DesisionTree
import Csv
import RandomForest

main :: IO () 
main = do
    input <- getDataFromCsv "iris.csv" 4
    forest <- train input 25 3
    print $ predictForest forest [Continous 5.9, Continous 3, Continous 5.1, Continous 1.8]

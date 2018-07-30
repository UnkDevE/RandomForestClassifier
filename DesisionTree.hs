{-# LANGUAGE ExistentialQuantification #-}

module DesisionTree
(
)
where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

data Feature = Continuous Double | Discrete String deriving (Eq, Show)

column :: [[a]] -> Int -> [a]
column grid index = foldr (\xs acc -> xs!!index:acc) [] grid

pmf :: [[Feature]] -> Int -> Feature -> Double
pmf trainingData feature value = 
    fromIntegral (length $ filter (==value) col) / (fromIntegral $ length col)
    where col = column trainingData feature

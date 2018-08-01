{-# LANGUAGE ExistentialQuantification #-}

module DesisionTree
(
)
where

import Data.List

data Feature = Continuous Double | Discrete String deriving (Eq, Show)

column :: [[a]] -> Int -> [a]
column grid index = foldr (\xs acc -> xs!!index:acc) [] grid

pmf :: [[Feature]] -> Int -> Feature -> Double
pmf trainingData feature value =
    fromIntegral (length $ filter (==value) col) / (fromIntegral $ length col)
    where col = column trainingData feature

entropy :: [[Feature]] -> Int -> Feature -> Double
entropy trainingData feature value =
    negate $ (pmf trainingData feature value) * (logBase 2 $ pmf trainingData feature value)

types :: [[Feature]] -> Int -> [Feature]
types trainingData feature = nub $ column trainingData feature

featureEntropyProg :: (Feature -> Double -> Double) -> [[Feature]] -> Int -> Double
featureEntropyProg f trainingData feature =
    foldr (\x acc -> acc + (f x $ entropy trainingData feature x)) 0 $ types trainingData feature

featureEntropy :: [[Feature]] -> Int -> Double
featureEntropy = featureEntropyProg (\_ x -> x)

informationGain :: ([[Feature]], Int) -> Int -> Double
informationGain trainingData feature =
    (featureEntropy (fst trainingData) $ snd trainingData) -
    (featureEntropyProg (\x h -> h * (pmf (fst trainingData) feature x)) (fst trainingData) feature)

getBestFeature :: ([[Feature]], Int) -> Int
getBestFeature trainingData = foldr
    (\i acc -> if informationGain trainingData acc < informationGain trainingData i then i else acc) 0
    $ filter (/= snd trainingData) [1..((length $ head $ fst trainingData) - 1)]

getContinousSplit :: [[Feature]] -> Int -> Feature
getContinousSplit trainingData feature = foldr
    (\i acc -> if entropy trainingData feature i < entropy trainingData feature acc then i else acc )
    (head t) $ tail t
    where t = types trainingData feature


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

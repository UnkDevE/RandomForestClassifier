{-# LANGUAGE ExistentialQuantification #-}

module DesisionTree
(
)
where

import Data.List

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

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

featureEntropyProg :: (Feature -> Double -> Double) -> [[Feature]] -> Int -> Double
featureEntropyProg f trainingData feature =
    foldr (\x acc -> acc + (f x $ entropy trainingData feature x)) 0 $ nub $ column trainingData feature

featureEntropy :: [[Feature]] -> Int -> Double
featureEntropy = featureEntropyProg (\_ x -> x)

informationGain :: ([[Feature]], Int) -> Int -> Double
informationGain trainingData feature =
    (featureEntropy (fst trainingData) $ snd trainingData) -
    (featureEntropyProg (\x h -> h * (pmf (fst trainingData) feature x)) (fst trainingData) feature)

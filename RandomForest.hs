module RandomForest
(
)
where

import DeisisonTree
import System.Random
import Data.List.Split

type Forest a = [Tree a]

mode :: [a] -> a
mode xs = foldr (\x acc -> if length $ filter (==acc) xs < length $ filter (==x) xs then x else acc) (head xs) $ tail xs

pmf :: [a] -> a -> Double 
pmf xs x = (fromIntegral $ length $ filter (==x) xs) / (fromIntegral $ length xs)

predictForest :: Forest (Feature, Int) -> [Feature] -> ((Feature, Double)
predictForest forest input = (fst feature, pmf results feature)
    where results = map (\tree -> predict tree input) forest
          feature = mode results

train :: ([[Feature]], Int) -> Int -> Int -> IO Forest (Feature, Int)
train trainingData models nFeatures = do
    subs <- subsets trainingData models nFeatures
    return map trainTree subs

subsets :: ([[Feature]], Int) -> Int -> Int ->  IO [([[Feature]], Int)]
subsets (features, out) models nFeatures
    | nFeatures > length $ head features = error "nFeatures greater than features available"
    | otherwise = do
        let s = split out feature
        let trainingFeatures = (fst s) ++ (snd s)
        return
            [(features!!out:xs, 0) | xs <- repeat $ columns $ systematicSample nFeatures $ columns trainingFeatures]

systematicSample :: Int -> [a] -> IO [a]
systematicSample samples xs = do
    let interval = div (length xs) samples
    let start = randomRIO (1, interval)
    return [ x |  y <- [start..] x <- xs, rem y interval == 0]

columns :: [[a]] -> [[a]]
columns xxs = foldr (\xs acc -> combine acc (chunksOf 1 xs)) (chunksOf 1 $ head xxs) $ tail xxs

combine :: [[a]] -> [[a]] -> [[a]]
combine xs ys =
    foldr (\xs acc -> (fst xs) (snd xs):acc) [] $ map (\xs -> ((++) fst xs, snd xs)) $ zip xs ys


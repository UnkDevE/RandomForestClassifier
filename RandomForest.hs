module RandomForest
(
)
where

import DeisisonTree
import System.Random
import Data.List.Split

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


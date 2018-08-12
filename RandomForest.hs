module RandomForest
(
    predictForest,
    train
)
where

import DesisionTree
import System.Random (randomRIO)

type Forest a = [Tree a]

predictForest :: Forest (Feature, Int) -> [Feature] -> (Feature, Double)
predictForest forest input = (fst feature, pmf results feature)
    where results = map (\tree -> predict tree input) forest
          feature = mode results

mode :: (Eq a) => [a] -> a
mode xs = foldr (\x acc -> if (length $ filter (==acc) xs) < (length $ filter (==x) xs) then x else acc) (head xs) $ tail xs

pmf :: (Eq a) => [a] -> a -> Double
pmf xs x = (fromIntegral $ length $ filter (==x) xs) / (fromIntegral $ length xs)

train :: ([[Feature]], Int) -> Int -> Int -> IO (Forest (Feature, Int))
train trainingData models nFeatures = do
    subs <- subsets trainingData models nFeatures
    return (map trainTree subs)

subsets :: ([[Feature]], Int) -> Int -> Int ->  IO [([[Feature]], Int)]
subsets (features, out) 0 nFeatures
    | nFeatures > (length $ head features) = error "nFeatures greater than features available"
    | otherwise = do
        let s = split out features
        let trainingFeatures = (fst s) ++ (snd s)
        sample <- systematicSample nFeatures $ columns trainingFeatures
        return [(features!!out:(columns sample), 0)]

subsets tData@(features, out) models nFeatures
    | nFeatures > (length $ head features) = error "nFeatures greater than features available"
    | otherwise = do
        let s = split out features
        let trainingFeatures = (fst s) ++ (snd s)
        sample <- systematicSample nFeatures $ columns trainingFeatures
        xs <- subsets tData (models-1) nFeatures
        return ((features!!out:(columns sample), 0):xs)

systematicSample :: Int -> [a] -> IO [a]
systematicSample samples xs = do
    let interval = div (length xs) samples
    start <- randomRIO (1, interval)
    return [ x |  y <- [start..], x <- xs, rem y interval == 0]


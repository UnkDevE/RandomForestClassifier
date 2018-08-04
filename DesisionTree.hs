{-# LANGUAGE ExistentialQuantification #-}

module DesisionTree
(
    Feature (..),
    Tree (..),
    evaluateTree,
    id3
)
where

import Data.List (nub)
import Data.Maybe (fromJust)

readMay :: (Read a) => String -> Maybe a
readMay s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing

data Feature = Continous Double | Discrete String deriving (Eq, Ord, Show)

instance Read Feature where
    readsPrec _ a = let r = readMay a :: Maybe Double in 
                    case r of 
                        Nothing -> [(Discrete a, "")]
                        _ -> [(Continous $ fromJust r, "")]

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

quicksort :: (Ord b) => [(a, b)] -> [(a, b)]
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, snd a <= snd x]
        larger = [a | a <- xs, snd a > snd x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

getSplitOrder :: [[Feature]] -> Int -> [(Feature, Int)]
getSplitOrder trainingData feature =
    zip (fst $ unzip $ quicksort $ zip t $ map (entropy trainingData feature) t) $ repeat feature
    where t = types trainingData feature

data Tree a = Node a [Tree a] deriving (Eq, Show)

data Crumb a = NCrumb a [Tree a] [Tree a] deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

split :: Int -> [a] -> ([a], [a])
split i xs = (snd $ unzip $ filter (\a -> fst a < i) zxs, snd $ unzip $ filter (\a -> i > fst a) zxs)
    where zxs = zip [0..] xs

treeTo :: Zipper a -> Int -> Maybe (Zipper a)
treeTo (Node x xs, bs) branch
    | 0 < branch && branch < length xs = Just (xs!!branch, NCrumb x (fst splitxs) (snd splitxs):bs)
    | otherwise = Nothing
    where splitxs = split branch xs

goRight :: Zipper a -> Maybe (Zipper a)
goRight (item, NCrumb x ls (nitem:rs):bs) = Just (nitem, NCrumb x (ls ++ [item]) rs:bs)
goRight (item, NCrumb x ls []:bs) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (item, NCrumb x ls rs:bs) = Just (Node x (ls ++ [item] ++ rs), bs)
goUp (item, []) = Nothing

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Node x (item:xs), bs) = Just (item, NCrumb x (fst splitxs) (snd splitxs):bs)
    where splitxs = split 1 xs
goDown (Node x [], bs) = Nothing

next :: (Eq a) => Zipper a -> Maybe (Zipper a)
next zipper
    | right /= Nothing = right
    | otherwise = goUp zipper
    where right = goRight zipper

insertLevel :: Zipper a -> [a] -> Zipper a
insertLevel (Node x _, bs) xs = (Node x (map (\x -> Node x []) xs), bs)

isDiscrete :: Feature -> Bool
isDiscrete (Discrete _) = True
isDiscrete _ = False

evaluate :: ([[Feature]], Int) -> (Feature, Int) -> ([[Feature]], Int)
evaluate (features, out) (desision, feature)
    | isDiscrete desision = (filter (\xs -> xs!!feature == desision) features, out)
    | otherwise = (filter (\xs -> xs!!feature <= desision) features, out)

evaluateTree :: ([[Feature]], Int) -> Zipper (Feature, Int) -> ([[Feature]], Int)
evaluateTree trainingData zipper@(Node x _, bs)
    | up /= Nothing = evaluateTree eval $ fromJust up
    | otherwise = eval
    where up = goUp zipper
          eval = evaluate trainingData x

getOutputSet :: ([[Feature]],Int) -> Zipper (Feature, Int) -> [Feature]
getOutputSet trainingData@(feature, out) zipper =
    column (fst $ evaluateTree trainingData zipper) out

id3 :: ([[Feature]], Int) -> Zipper (Feature, Int) -> Zipper (Feature, Int)
id3 trainingData@(features, out) prevZipper@(Node x _, bs)
    | (length $ nub outSet) == 1 = let newZipper = (Node x [Node (head outSet, out) []], bs)
                                       nextZipper = next newZipper
                                 in if nextZipper /= Nothing then id3 (features, out) (fromJust nextZipper) else newZipper
    | otherwise =
        id3 newFeatures
            $ fromJust $ goDown $ insertLevel prevZipper $ getSplitOrder (fst newFeatures) $ getBestFeature newFeatures
    where outSet = getOutputSet trainingData prevZipper
          splitF = split out features
          newFeatures = ((fst splitF) ++ [outSet] ++ (snd splitF), out)

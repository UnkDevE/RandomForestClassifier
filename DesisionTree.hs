module DesisionTree
(
    Feature (..),
    Tree (..),
    evaluateZipper,
    trainTree,
    predict,
    predictFail,
    split,
    columns
)
where

import Data.List.Split (chunksOf)
import Data.List (nub)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

readMay :: (Read a) => String -> Maybe a
readMay s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing

data Feature = Continous Double | Discrete String | Any deriving (Eq, Ord, Show)

instance Read Feature where
    readsPrec _ a = let r = readMay a :: Maybe Double in
                    case r of
                        Nothing -> [(Discrete a, "")]
                        _ -> [(Continous $ fromJust r, "")]

column :: [[a]] -> Int -> [a]
column grid index = foldr (\xs acc -> xs!!index:acc) [] grid

columns :: [[a]] -> [[a]]
columns xxs = foldr (\xs acc -> combine acc (chunksOf 1 xs)) (chunksOf 1 $ head xxs) $ tail xxs

combine :: [[a]] -> [[a]] -> [[a]]
combine xs ys =
    foldr (\xs acc -> (fst xs) (snd xs):acc) [] $ map (\xs -> ((++) (fst xs), snd xs)) $ zip xs ys

pmf :: [[Feature]] -> Int -> Feature -> Double
pmf trainingData feature value =
    fromIntegral (length $ filter (==value) col) / (fromIntegral $ length col)
    where col = column trainingData feature

types :: [[Feature]] -> Int -> [Feature]
types trainingData feature = nub $ column trainingData feature

fairEntropy :: [[Feature]] -> Int -> Feature -> Double
fairEntropy trainingData feature value =
    negate $ (pmf trainingData feature value) * (logBase 2 $ pmf trainingData feature value)

entropy :: [[Feature]] -> Int -> Double
entropy trainingData feature =
    foldr (\x acc -> acc + fairEntropy trainingData feature x) 0 $ types trainingData feature

featureEntropy :: ([[Feature]], Int) -> Int -> Double
featureEntropy (trainingData, outFeature) feature =
    foldr (\x acc -> acc + (pmf trainingData feature (fst x)) * entropy (snd x) outFeature)
        0 $ zip
            (types trainingData feature)
            $ map (\t -> filter (\d -> d!!feature == t) trainingData) $ types trainingData feature


informationGain :: ([[Feature]], Int) -> Int -> Double
informationGain trainingData feature =
    (entropy (fst trainingData) $ snd trainingData) - featureEntropy trainingData feature

getBestFeature :: ([[Feature]], Int) -> Int
getBestFeature trainingData = foldr
    (\i acc -> if (informationGain trainingData acc < informationGain trainingData i) then i else acc) 0
    $ filter (/= snd trainingData) [1..((length $ head $ fst trainingData) - 1)]

quicksort :: (Ord b) => [(a, b)] -> [(a, b)]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, snd a <= snd x]
        larger = [a | a <- xs, snd a > snd x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

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
goRight (item, []) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (item, NCrumb x ls rs:bs) = Just (Node x (ls ++ [item] ++ rs), bs)
goUp (item, []) = Nothing

root :: (Eq a) => Zipper a -> Zipper a
root zipper
    | up /= Nothing = root $ fromJust up
    | otherwise = zipper
    where up = goUp zipper

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Node x (item:xs), bs) = Just (item, NCrumb x [] (xs):bs)
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

isContinous :: Feature -> Bool
isContinous (Continous _) = True
isContinous _ = False

evaluateData :: ([[Feature]], Int) -> (Feature, Int) -> ([[Feature]], Int)
evaluateData (features, out) (desision, feature)
    | isDiscrete desision = (filter (\xs -> xs!!feature == desision) features, out)
    | isContinous desision = (filter (\xs -> xs!!feature <= desision) features, out)
    | otherwise = (features, out)

evaluateZipper :: ([[Feature]], Int) -> Zipper (Feature, Int) -> ([[Feature]], Int)
evaluateZipper trainingData zipper@(Node x _, bs)
    | up /= Nothing = evaluateZipper eval $ fromJust up
    | otherwise = eval
    where up = goUp zipper
          eval = evaluateData trainingData x

quantiles :: (Eq a) => [a] -> [a]
quantiles xs =  map (\p -> xs!!(fromIntegral $ ceiling $ p * (fromIntegral $ length $ nub xs))) [0.25, 0.5..1]

quantileBin :: ([[Feature]], Int) -> ([[Feature]], Int)
quantileBin (tData, outFeature) =
    (columns $
        cols!!outFeature:(
            map
                (\col -> map (\feature -> foldr (\x acc -> if feature <= acc then x else acc) (Continous 0) $ quantiles col) col)
                $ filter (\col -> isContinous $ head col) $ excluding cols outFeature
            )
    , 0)
    where cols = tData

excluding :: [a] -> Int -> [a]
excluding xs n = fst $ unzip $ filter (\x -> snd x /= n) $ zip xs [0..]

id3 :: ([[Feature]], Int) -> Zipper (Feature, Int) -> Zipper (Feature, Int)
id3 trainingData@(features, out) prevZipper@(Node x _, bs)
    | (length $ nub outSet) == 1 = let newZipper = (Node x [Node (head outSet, out) []], bs)
                                       nextZipper = next newZipper
                                 in if nextZipper /= Nothing then id3 (features, out) (fromJust nextZipper) else newZipper
    | otherwise =
        id3 newFeatures
            $ fromJust $ goDown $ insertLevel prevZipper $ zip (types (fst newFeatures) $ bestF) $ repeat bestF
    where newFeatures = evaluateZipper trainingData prevZipper
          outSet = column (fst newFeatures) out
          bestF = getBestFeature newFeatures

id3Debug :: ([[Feature]], Int) -> Zipper (Feature, Int) -> Zipper (Feature, Int)
id3Debug trainingData@(features, out) prevZipper@(Node x _, bs)
    | (length $ nub outSet) == 1 = let newZipper = (Node x [Node (head outSet, out) []], bs)
                                       nextZipper = next newZipper
                                 in if nextZipper /= Nothing then trace
                                    ("classified. nextZipper" ++ show nextZipper)
                                    id3Debug (features, out) (fromJust nextZipper) else newZipper
    | otherwise =
        trace
        ("classifying newFeatures Length " ++ show (length $ fst newFeatures) ++ " OutSet types: " ++ show (length $ nub outSet)
           ++ "best Feature" ++ show (getBestFeature newFeatures))
        id3Debug newFeatures
            $ fromJust $ goDown $ insertLevel prevZipper $ zip (types (fst newFeatures) $ bestF) $ repeat bestF
    where newFeatures = evaluateZipper trainingData prevZipper
          outSet = column (fst newFeatures) out
          bestF = getBestFeature newFeatures

toTree :: (Eq a) => Zipper a -> Tree a
toTree zipper = fst $ root zipper

trainTree :: ([[Feature]], Int) -> Tree (Feature, Int)
trainTree trainingData = toTree $ id3 (quantileBin trainingData) (Node (Any, 0) [], [])

trainTreeDebug :: ([[Feature]], Int) -> Tree (Feature, Int)
trainTreeDebug trainingData = toTree $ id3Debug (quantileBin trainingData) (Node (Any, 0) [], [])

eval :: Zipper (Feature, Int) -> Feature -> Bool
eval (Node x _, _) feature
    | isDiscrete $ fst x = feature == (fst x)
    | isContinous $ fst x = feature <= (fst x)
    | otherwise = True

navigate :: Zipper (Feature, Int) -> [Feature] -> Zipper (Feature, Int)
navigate zipper@(Node (_, nfeature) _, _) input
    | eval zipper (input!!nfeature) = let down = goDown zipper
                                    in if down /= Nothing then navigate (fromJust down) input else zipper
    | otherwise = let n = next zipper
                  in if n /= Nothing then navigate (fromJust n) input else zipper

getNode :: Tree a -> a
getNode (Node x _) = x

predict :: Tree (Feature, Int) -> [Feature] -> (Feature, Int)
predict tree input = getNode $ fst $ navigate (tree, []) input

predictFail :: ([[Feature]], Int) -> (Feature, Int) -> Bool
predictFail (_, outf) (_, fNo) = outf == fNo

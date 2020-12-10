module Main where
import Funs
import Control.Monad

hasSumIn :: (Eq a,Num a)=> [a] -> a -> Bool
hasSumIn [] _ = False
hasSumIn (x:xs) needle =
    (needle-x) `elem` xs || hasSumIn xs needle


doTheThing :: [Int] -> Int -> Maybe Int
doTheThing nums cnt
    | length nums <= cnt = Nothing
    | not (hasSumIn prefix needle) = Just needle
    | otherwise = doTheThing (tail nums) cnt
    where
        (prefix,suffix) = splitAt cnt nums
        needle = head suffix

sumsFrom :: [Int] -> [[Int]]
sumsFrom nums = mapM sumsFrom' [1..(length nums)] nums

sumsFrom' cnt nums
    | cnt > (length nums) = []
    | otherwise = (sum (take cnt nums)):(sumsFrom' cnt (tail nums))

findElem :: Eq a => a -> [a] -> Maybe Int
findElem e xs = findElem' 0 xs where
    findElem' acc [] = Nothing
    findElem' acc (x:xs) 
        | x==e = Just acc
        | otherwise = findElem' (acc+1) xs

findSubsequence :: Int -> [Int] -> [(Int,Int)]
findSubsequence key nums = findSubsequence' 0 table where
    findSubsequence' acc [] = []
    findSubsequence' acc (t:ts) = case findElem key t of
        Just row -> (row,acc):findSubsequence' (acc+1) ts
        Nothing -> findSubsequence' (acc+1) ts
    table = sumsFrom nums

getSequence :: (Int,Int) -> [Int] -> [Int]
getSequence (start,len) lon = take (len+1) $ drop start lon


main = do
    inp <- readLoA :: IO [Int]
    let key = doTheThing inp 25
    print key
    let seqids = findSubsequence<$>key<*>Just inp
    let seq = getSequence<$>((!!)<$>seqids<*>Just 1)<*>Just inp
    let smin = (foldr1 min)<$>seq
    let smax = (foldr1 max)<$>seq
    print $ (+)<$>smin<*>smax
    return ()


module Main (main) where
import Funs
import Debug.Trace

findInList :: Eq a => [a] -> a -> Maybe a
findInList [] _ = Nothing
findInList (x:xs) need =
    if x == need then Just x else findInList xs need


findSum :: [Integer] -> Integer -> Maybe (Integer,Integer)
findSum (x:xs) i =
    case findInList xs (i-x) of
        Nothing -> findSum xs i
        Just y -> Just (x,y)
findSum _ _ = Nothing

find3Sum :: [Integer] -> Integer -> Maybe (Integer,Integer,Integer)
find3Sum (x:xs) i =
    case findSum xs (i-x) of
        Nothing -> find3Sum xs i
        Just (y,z) -> Just (x,y,z)
find3Sum _ _ = Nothing


-- part 1
_main = do
    inp <- readLoA :: IO [Integer]
    print $ findSum inp 2020

-- part 2
main = do
    inp <- readLoA :: IO [Integer]
    print $ find3Sum inp 2020

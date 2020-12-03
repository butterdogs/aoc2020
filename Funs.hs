module Funs where

readLines :: IO [String]
readLines = do
    l <- getLine
    case l of
        "" -> return []
        l -> (l:)<$>readLines

readLoA :: Read a => IO [a]
readLoA = do
    l <- getLine
    case l of
        "" -> return []
        l -> (x:)<$>readLoA where x = read l

splitOn :: (a -> Bool) -> [a] -> ([a],[a])
splitOn _ [] = ([],[])
splitOn f (x:xs) =
    if f x then ([],xs)
    else (x:xs',xs'') where
        (xs',xs'') = splitOn f xs

splitOnChar c = splitOn (\x -> x == c)

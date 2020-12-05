module Main where
import Funs

import Data.List


powers n = n**2

-- sid :: String -> (Int,Int)
seat s = (toBin (take 7 s), toBin (drop 7 s)) where 
        toBin str = sum $ zipWith (*) (powers (length str)) (map isPos str)
        isPos c = if (c=='B' || c=='R') then 1 else 0
        powers n = map (2^) [n-1,n-2..0]

sid :: String -> Int
sid s = let
    (row,col) = seat s
    in 8*row+col

findGaps :: [Int] -> [Int]
findGaps (x:y:xs)
    | y == (x+2) = (x+1):fg'
    | otherwise = fg'
    where fg' = findGaps (y:xs)
findGaps _ = []

main = do
    inp <- readLines
    let sids = map sid inp
    print $ foldr max 0 sids -- part 1

    print $ findGaps $ sort sids -- part 2
    return ()

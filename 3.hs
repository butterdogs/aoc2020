module Main where
import Funs

sled :: [String] -> Int -> Int -> Int -> Int
sled [] _ _ _ = 0
sled xs pos right down = 
    (if ((head xs)!!pos) == '#' then 1 else 0)
    +
    sled (drop down xs) (pos+right) right down



main = do
    lines <- readLines
    let cycled = map cycle lines
    let sled' = sled cycled 0
    print $ sled' 1 1
    print $ sled' 3 1 -- should be 7
    print $ sled' 5 1
    print $ sled' 7 1
    print $ sled' 1 2

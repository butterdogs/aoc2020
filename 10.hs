module Main where
import Funs
import Data.List


gaps lon = zipWith (-) (tail lon) lon

combos :: [Int]
combos = (1:1:2:next combos) where
    next (x:y:z:xs) = (x+y+z):next(y:z:xs)

figureWays :: [Int] -> Int
figureWays diffs = let
    groups = group diffs
    ones = filter ((==1).head) groups :: [[Int]]
    ezcombos i = combos!!i
    in foldr1 (*) (map (ezcombos.length) ones)


main = do
    inp <- readLoA :: IO [Int]
    let sorted = sort inp
    let sorted' = 0:sorted ++ [(last sorted)+3]
    let diffs = gaps sorted'
    let groupd = group $ sort diffs
    print $ map length groupd -- part 1
    print $ figureWays diffs -- part 2
    return ()

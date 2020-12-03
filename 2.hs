module Main where
import Funs
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec -- for lift?
import Data.Char -- for isDigit and isAlpha
import Debug.Trace
import GHC.Read -- for readPrec

digit :: ReadP Char
digit = satisfy isDigit

letter :: ReadP Char
letter = satisfy isAlpha




data PWLine = PWLine Int Int Char String deriving Show
instance Read PWLine where
    readPrec = lift pwline

valid :: PWLine -> Bool
valid (PWLine i1 i2 c str) = let
    count = length $ filter (\x -> x==c) str
    in count >= i1 && count <= i2

valid2 :: PWLine -> Bool
valid2 (PWLine i1 i2 c str) = let
    one = str!!(i1-1) == c
    two = str!!(i2-1) == c
    in
    (one || two) && (not (one && two))


pwline :: ReadP PWLine
pwline = do
    i1 <- many digit
    char '-'
    i2 <- many digit
    skipSpaces
    c <- letter
    char ':'
    skipSpaces
    str <- many letter
    return $ PWLine (read i1) (read i2) c str

main :: IO ()
main = do
    lines <- readLoA :: IO [PWLine]
    print $ length $ filter valid lines --part 1
    print $ length $ filter valid2 lines -- part 2



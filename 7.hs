{-# LANGUAGE FlexibleContexts #-}
module Main where
import Funs
import Text.Parsec
import Text.Parsec.Char
import Debug.Trace
import Data.List

data BagRule = BagRule String [(Int, String)] deriving Show

rule1 = "light red bags contain 1 bright white bag, 2 muted yellow bags."
rule2 = "bright white bags contain 1 shiny gold bag."
rule3 = "dotted black bags contain no other bags."

tryParse p = do
    real <- p
    rest <- many anyChar
    return $ (real,rest)

pBagWithColor :: Stream s m Char => ParsecT s u m String
pBagWithColor = do
    color1 <- manyTill anyChar (try ((string " bag")>>(optional (char 's'))))
    return color1

pCBags :: Stream s m Char => ParsecT s u m (Maybe (Int, String))
pCBags = 
    (try noBags) <|> bagString where
    noBags = do
        try $ string "no other bags"
        return Nothing
    bagString = do
        spaces
        num <- many digit
        spaces
        color <- pBagWithColor
        return $ Just ((read num), color)


pBagRule :: Stream s m Char => ParsecT s u m BagRule
pBagRule = do
    color1 <- pBagWithColor
    string " contain "
    rules <- sepBy1 pCBags (char ',')
    let realRules = [x | Just x <- rules]
    rest <- many anyChar
    return $ BagRule color1 realRules

bagIsColor :: String -> BagRule -> Bool
bagIsColor color (BagRule c _) = c==color

canHold :: BagRule -> BagRule -> Bool
canHold (BagRule str _) (BagRule _ bags) = canHold' str bags where
    canHold' color rules = any (\(num,c)->c==color) rules


reachableFrom :: [BagRule] -> [BagRule] -> ([BagRule],[BagRule])
reachableFrom [] unreached = ([],unreached)
reachableFrom (b:bs) unreached = let
    (newBags,rest) = partition (canHold b) unreached
    (nextreach,rest') = reachableFrom (bs++newBags) rest
    in (b:nextreach,rest')


countBags :: BagRule -> [BagRule] -> Int
countBags (BagRule _ []) pool = 1
countBags (BagRule _ rules) pool = let
    countFromRule (i,color) = let
        matches = filter (\(BagRule c rs)->c==color) pool
        recur = map ((flip countBags)pool) matches
        in i * (sum recur)
    in 1 + (sum $ map countFromRule rules)

-- 34864 (too high)

main = do
    inp <- readLines
    let parsed = map (\x -> parse pBagRule "" x) inp
    let nerror = [x | Right x <- parsed ]
    let (gold,rest) = partition (bagIsColor "shiny gold") nerror
    -- print (gold,rest)
    let (goldreach,rest') = reachableFrom gold rest
    print $ (length goldreach) - 1
    print $ countBags (head gold) rest - 1
    return ()


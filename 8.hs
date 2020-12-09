module Main where
import Funs
import Text.Parsec
import Text.Parsec.Char
import Control.Monad
import Data.Either

data Cmd = Nop Int | Acc Int | Jmp Int deriving Show

doOp :: Cmd -> Int -> Int -> (Int,Int)
doOp (Nop i) acc ip = (acc, ip+1)
doOp (Acc i) acc ip = (acc+i, ip+1)
doOp (Jmp i) acc ip = (acc, ip+i)


nop = do
    string "nop"
    space
    num <- cmdNumber
    return $ Nop num

cmdNumber = do
    sign <- (char '-') <|> (char '+')
    digits <- many digit
    let op = if sign == '-' then negate else id
    return $ op (read digits)

jmp = do
    string "jmp"
    space
    num <- cmdNumber
    return $ Jmp num

acc = do
    string "acc"
    space
    num <- cmdNumber
    return $ Acc num


cmd = nop <|> acc <|> jmp

run :: [Cmd] -> Either Int Int
run cmds = run' 0 0 [] where
    len = length cmds
    run' acc ip visited
        | ip < 0 || ip > len = Left acc --off the end
        | ip == len = Right acc -- off the end
        | ip `elem` visited = Left acc -- infinite loop
        | otherwise = let
            (acc',ip') = doOp (cmds!!ip) acc ip
            in run' acc' ip' (ip:visited)

pastaBilities :: [Cmd] -> [[Cmd]]
pastaBilities [] = [[]]
pastaBilities (x:xs) = case x of
    a@(Acc i) -> (a:)<$>pastaBilities xs
    n@(Nop i) ->
        ((Jmp i):xs):((n:)<$>pastaBilities xs)
    j@(Jmp i) ->
        ((Nop i):xs):((j:)<$>pastaBilities xs)

findGoodExec :: [Cmd] -> [Int]
findGoodExec cmds =
    rights $ map run (pastaBilities cmds)

-- 1274 : too high
main = do
    inp <- readLines
    let cmds = map (parse cmd "") inp
    let goodcmds = rights cmds
    print $ run goodcmds -- part i
    print $ findGoodExec goodcmds

    return ()

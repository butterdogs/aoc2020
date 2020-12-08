module Main where
import Funs
import qualified Data.Set as S
import Data.List --unfoldr

doLines :: [String] -> Maybe ([String],[String])
doLines ss = case splitOn (=="") ss of
    ([],ss) -> Nothing
    (ss',ss) -> Just (ss',ss)


testStr = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"


groupedLines :: String -> [[[Char]]]
groupedLines inp = unfoldr doLines (lines inp)

bigLines :: String -> [String]
bigLines = (map concat).groupedLines

-- any in group
groupInp :: String -> [S.Set Char]
groupInp inp = map (foldr S.insert S.empty) (bigLines inp)

-- all in group
groupInp' :: String -> [S.Set Char]
groupInp' inp = let
    groups = groupedLines inp :: [[[Char]]]
    sets = map (map S.fromList) groups :: [[S.Set Char]]
    merged = map (foldr1 S.intersection) sets
    in merged


process1 inp = show $ foldr ((+).(S.size)) 0 $ groupInp inp
process2 inp = show $ foldr ((+).(S.size)) 0 $ groupInp' inp

main = do
    interact process2

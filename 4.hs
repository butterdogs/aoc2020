{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import Funs
import qualified Data.Set as S
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Debug.Trace


reqs = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] --omit "cid"

data Height = Centimeters Int | Inches Int deriving Show

validHt (Centimeters i) = i >= 150 && i <= 193
validHt (Inches i) = i >= 59 && i <= 76

parseHt = do
    num <- many digit
    let inum = read num
    str <- string "cm" <|> string "in"
    case str of
        "cm" -> return $ Centimeters inum
        "in" -> return $ Inches inum

bad _ = False
good _ = True
ifParse parser str = either bad good $ (parse parser "" str)

validStr :: (String,String) -> Bool
validStr (k,v) = 
    case k of
    "byr" -> either bad (\x->x>=1920 && x<= 2002) $ fmap read $ parse (many digit) "" v
    "iyr" -> either bad (\x->x>=2010 && x<= 2020) $ fmap read $ parse (many digit) "" v
    "eyr" -> either bad (\x->x>=2020 && x<= 2030) $ fmap read $ parse (many digit) "" v
    "hgt" -> either bad validHt $ parse parseHt "" v
    "hcl" -> ifParse ((char '#') >> (count 6 hexDigit)) v
    "ecl" -> ifParse (choice (map (try.string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])) v
    "pid" -> ifParse (count 9 digit) v
    "cid" -> True
    _ -> False

valid :: S.Set (String,String) -> Bool
valid s = (S.difference reqs (S.map fst s) == S.empty)
          &&
          all id (S.map validStr s)

fields :: String -> S.Set (String,String)
fields = S.fromList.(map (splitOnChar ':') ).words

testStr = "aoeu\nxyz\n\nebc\nec"

bigLines :: String -> [String]
bigLines s = map (intercalate " ") $ unfoldr doLines (lines s)
    where
    doLines ss = case splitOn (=="") ss of
        ([],ss) -> Nothing
        (ss',ss) -> Just (ss',ss)

process :: String -> String
process inp = show $ length.filter id $ map (valid.fields) (bigLines inp)


valid1 = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
valid2 = "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
valid3 = "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"
valid4 = "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

quicktest s = map validStr $ S.toList $ traceShowId $ fields s

tests = [
    validStr ("byr","2002"),
    not (validStr ("byr","2003")),
    validStr ("hgt","60in"),
    validStr ("hgt","190cm"),
    not$validStr ("hgt","190in"),
    not$validStr ("hgt","190"),
    validStr ("hcl","#123abc"),
    not$validStr ("hcl","#123abz"),
    not$validStr ("hcl","123abz"),
    validStr ("ecl","brn"),
    not$validStr ("ecl","wat"),
    validStr ("pid","000000001"),
    not$validStr ("pid","0123456789")
    ]



main = do
    --print tests
    interact process
    --print ()

import Advent
import Data.Maybe (isJust)
import Control.Monad (guard)
import Data.Char (isDigit, isHexDigit, isSpace)

type Passport = [(String, String)]

main = runSoln' (parseAll pPassports) (countp valid) (countp valid')

valid :: Passport -> Bool
valid x = and $ map (`elem` map fst x) fs
  where
    fs = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

valid' :: Passport -> Bool
valid' x = and [
    "byr" ? rng 1920 2002,
    "iyr" ? rng 2010 2020,
    "eyr" ? rng 2020 2030,
    "hgt" ? checkHeight,
    "hcl" ? \x -> length x == 7 && head x == '#' && all isHexDigit (tail x),
    "ecl" ? (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
    "pid" ? \x -> length x == 9 && all isDigit x
  ]
  where
    k ? p = isJust $ lookup k x >>= guard . p

checkHeight :: String -> Bool
checkHeight xs = if u == "cm" then rng 150 193 n else rng 59 76 n
  where
    (n, u) = span isDigit xs

rng :: Int -> Int -> String -> Bool
rng l h x = n >= l && n <= h
  where
    n = read x

pPassports :: Parser [Passport]
pPassports = (pField `endBy` spaceChar) `sepBy` newline

pField :: Parser (String, String)
pField = (,) <$> many letterChar <* char ':' <*> many (satisfy (not . isSpace))

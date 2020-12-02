import Data.Maybe
import Soln

find :: Int -> [Int] -> Maybe Int
find n xs = listToMaybe $ filter (\x -> elem (n - x) xs) xs

part1 xs = a * (2020 - a) where Just a = find 2020 xs

part2 xs = a * b * (2020 - a - b)
  where
    a = head $ filter (\x -> isJust $ find (2020 - x) xs) xs
    Just b = find (2020 - a) xs

main = runSoln' (map read . lines) part1 part2

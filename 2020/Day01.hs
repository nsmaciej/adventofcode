import Advent
import Data.Maybe

main = runSoln' (map read . lines) part1 part2

sum2 :: Int -> [Int] -> Maybe Int
sum2 k xs = listToMaybe $ filter (\x -> elem (k - x) xs) xs

part1 :: [Int] -> Int
part1 xs = a * (2020 - a)
  where
    Just a = sum2 2020 xs

part2 :: [Int] -> Int
part2 xs = a * b * (2020 - a - b)
  where
    a = head $ filter (\x -> isJust $ sum2 (2020 - x) xs) xs
    Just b = sum2 (2020 - a) xs
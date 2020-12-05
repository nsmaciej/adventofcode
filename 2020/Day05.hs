import Advent
import Data.List
import Data.Maybe

main = runSoln' (map parse . lines) maximum part2

part2 :: [Int] -> Int
part2 xs = snd . fromJust $ find (uncurry (/=)) $ zip (sort xs) [minimum xs..]

parse :: String -> Int
parse x = 8 * locate 'F' 'B' 127 x + locate 'L' 'R' 7 x

locate :: Char -> Char -> Int -> String -> Int
locate lc hc = divide 0
  where
    divide l h (x:xs)
      | x == lc = divide l ((l + h) `div` 2) xs
      | x == hc = divide ((l + h) `div` 2) h xs
      | otherwise = divide l h xs
    divide _ h [] = h

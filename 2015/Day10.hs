import Advent
import Data.List

main = runSoln' id (solve 40) (solve 50)

solve :: Int -> String -> Int
solve n = length . (!! n) . iterate las

las :: String -> String
las = concatMap (\x -> show (length x) ++ [head x]) . group
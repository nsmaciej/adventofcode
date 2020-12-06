import Advent

main = runSoln' (combinations 150 0 .  map read . lines) length (\x -> countp (minimum x ==) x)

combinations :: Int -> Int -> [Int] -> [Int]
combinations 0 k _ = [k]
combinations _ _ [] = []
combinations n k (c:cs) = combinations (n - c) (k + 1) cs ++ combinations n k cs
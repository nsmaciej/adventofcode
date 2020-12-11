import Advent

main = runSoln' (map read . lines) part1 part2

part1 :: [Int] -> Int
part1 xs = k
  where
    ws = map (\x -> (x !! 25, take 25 x)) $ tails xs
    Just (k, _) = find (not . uncurry sums) ws

sums :: Int -> [Int] -> Bool
sums n ds = or [x + y == n | (x:xs) <- tails ds, y <- xs]

part2 :: [Int] -> Int
part2 ds = maximum rs + minimum rs
  where rs = head [ys | xs <- inits ds, ys <- tails xs, sum ys == part1 ds]
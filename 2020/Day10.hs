import Advent

main = runSoln' (map read . lines) part1 (const 0)

part1 :: [Int] -> Int
part1 ds = (x + 1) * (y + 1)
  where
    xs = sort ds
    [x, y] = map length . group . sort $ zipWith (-) (tail xs) xs
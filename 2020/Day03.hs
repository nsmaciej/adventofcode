import Advent

main = runSoln'
    lines
    (solve (3, 1))
    (\x -> product $ map (`solve` x) ds)
  where
    ds = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solve :: (Int, Int) -> [String] -> Int
solve (dx, dy) = f 0
  where
    f x (t : ts) =
      let k = if t !! (x `mod` length t) == '#' then 1 else 0
      in k + f (x + dx) (drop (dy - 1) ts)
    f _ [] = 0
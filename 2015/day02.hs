main :: IO ()
main = do
  xs <- readFile "inputs/day02.txt"
  let ds = map (map read . split 'x') $ lines xs
  print . sum $ map paper ds
  print . sum $ map ribbon ds

paper :: [Int] -> Int
paper [l, w, h] = minimum ds + (sum $ map (2 *) ds)
  where
    ds = [l * w, w * h, h * l]

ribbon :: [Int] -> Int
ribbon [l, w, h] = l * w * h + 2 * minimum [l + w, w + h, h + l]

split :: Eq a => a -> [a] -> [[a]]
split s xs = case span (/= s) xs of
  (l, []) -> [l]
  (l, r) -> l : (split s $ tail r)
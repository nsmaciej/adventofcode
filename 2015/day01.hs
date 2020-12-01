main :: IO ()
main = do
  i <- readFile "inputs/day01.txt"
  let xs = scanl (+) 0 $ map (\x -> if x == '(' then 1 else -1) $ init i
  print $ last xs
  print $ length $ takeWhile (/= -1) xs
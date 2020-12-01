import Soln

main = runSoln' parse last (length . takeWhile (/= -1))
  where
    parse = scanl (+) 0 . map (\x -> if x == '(' then 1 else -1) . init
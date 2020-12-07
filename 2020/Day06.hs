import Advent

main = runSoln'
  (parseAll $ (some letterChar `endBy` newline) `sepBy` newline)
  (sum . map (length . foldl1' union))
  (sum . map (length . foldl1' intersect))
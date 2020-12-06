import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List

main = runSoln'
  (parseString $ (some letterChar `endBy` newline) `sepBy` newline)
  (sum . map (length . foldl1' union))
  (sum . map (length . foldl1' intersect))
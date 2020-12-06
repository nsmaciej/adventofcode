import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List

main = runSoln'
  (parseString pAll)
  (sum . map (length . nub . concat))
  (sum . map (length . foldr1 intersect))

pAll :: Parser [[String]]
pAll = pGroup `sepBy` newline

pGroup :: Parser [String]
pGroup = some letterChar `endBy` newline
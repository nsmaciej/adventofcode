import Advent
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I

main = runSoln'
  (parseAll $ decimal `sepBy` char ',')
  ((!! 2019) . play)
  ((!! 29999999) . play)

play :: [Int] -> [Int]
play xs = xs ++ (tail . map fst $ scanl fold (last xs, I.fromList (zip (init xs) [0..])) [length xs - 1..])
  where
    fold :: (Int, IntMap Int) -> Int -> (Int, IntMap Int)
    fold (prev, seen) turn
      | Just last <- seen I.!? prev = (turn - last, I.insert prev turn seen)
      | otherwise = (0, I.insert prev turn seen)
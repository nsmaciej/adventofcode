import Advent
import Data.Function (on)

type Input = (Int, [Int])

main = runSoln' (parseAll pInput) part1 (const 0)

part1 :: Input -> Int
part1 (t, xs) = bus * (next bus - t)
  where
    next x = x * (1 + t `div` x)
    bus = minimumBy (compare `on` next) xs

pInput :: Parser Input
pInput = (,)
  <$> decimal <* newline
  <*> decimal `sepEndBy` some (oneOf "x,") <* newline
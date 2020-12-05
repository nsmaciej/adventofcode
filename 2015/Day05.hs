import Advent
import Data.List

main = runSoln' lines (countp nice) (countp nice')

nice :: String -> Bool
nice x =
  countp (`elem` "aeiou") x > 2
  && and (zipWith (==) x $ tail x)
  && not (any (`isInfixOf` x) ["ab", "cd", "pq", "xy"])

nice' :: String -> Bool
nice' x = twoPairs x && twoGap x

twoPairs :: String -> Bool
twoPairs (a:b:xs) = [a, b] `isInfixOf` xs || twoPairs (b:xs)
twoPairs _ = False

twoGap :: String -> Bool
twoGap (a:x:b:xs) = a == b || twoGap (x:b:xs)
twoGap _ = False
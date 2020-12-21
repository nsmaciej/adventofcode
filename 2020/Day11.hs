import Advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Point = (Int, Int)
type Seats = Map Point Seat
data Seat = Empty | Full deriving (Show, Eq)
main = runSoln' parse part1 (const 0)

part1 :: Seats -> Int
part1 w
  | w == step w = M.size $ M.filter (== Full) w
  | otherwise = part1 (step w)

step :: Seats -> Seats
step w = M.mapWithKey update w
  where
    update p Empty = if occupied w p == 0 then Full else Empty
    update p Full = if occupied w p >= 4 then Empty else Full

occupied :: Seats -> Point -> Int
occupied w (x', y') = length [1 | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0, w M.!? (x' + x, y' + y) == Just Full]

parse :: String -> Seats
parse str = M.fromList [((y, x), Empty) | (y, xs) <- zip [0..] (lines str), (x, i) <- zip [0..] xs, i == 'L']
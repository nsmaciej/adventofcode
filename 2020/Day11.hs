import Advent
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Maybe

type Point = (Int, Int)
type Seats = Map Point Seat
data Seat = Empty | Full | Floor deriving (Show, Eq)
type Occupied = Seats -> Point -> Int

main = runSoln' parse (const 0) (solve $ step occupied' 5)

solve :: (Seats -> Seats) -> Seats -> Int
solve step w
  | w == step w = M.size $ M.filter (== Full) w
  | otherwise = solve step (step w)

step :: Occupied -> Int -> Seats -> Seats
step occupied emtpy w = M.mapWithKey update w
  where
    update p Empty = if occupied w p == 0 then Full else Empty
    update p Full = if occupied w p >= emtpy then Empty else Full
    update _ Floor = Floor

occupied :: Occupied
occupied w p = length [1 | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0, w !? ((x, y) +. p) == Just Full]

walk :: Seats -> Point -> Point -> Maybe Seat
walk w p delta = fromJust $ find (/= Just Floor) . map (w !?) . tail $ iterate (+. delta) p

occupied' :: Occupied
occupied' w p = length [1 | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0, walk w p (x, y) == Just Full]

parse :: String -> Seats
parse str = M.fromList [((y, x), if i == 'L' then Empty else Floor)
  | (y, xs) <- zip [0..] (lines str), (x, i) <- zip [0..] xs]
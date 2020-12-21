{-# LANGUAGE LambdaCase #-}
import Advent

type Inst = (Char, Int)
type Point = (Int, Int)
type Ship = (Point, Int)
type Ship' = (Point, Int, Point)

main = runSoln' (map parse . lines) part1 part2

part1 :: [Inst] -> Int
part1 = dist . fst . foldl' step ((0, 0), 90)

part2 :: [Inst] -> Int
part2 = dist . (\(x, _, _) -> x) . foldl' step' ((0, 0), 90, (-1, 10))

dist :: Point -> Int
dist (x, y) = abs x + abs y

infixl 6 +.
(+.) :: Point -> Point -> Point
(x, y) +. (x', y') = (x + x', y + y')

infixl 7 *:
(*:) :: Point -> Int -> Point
(x, y) *: n = (x * n, y * n)

dir :: Char -> Point
dir 'N' = (-1, 0)
dir 'E' = (0, 1)
dir 'S' = (1, 0)
dir 'W' = (0, -1)

rot :: Int -> Point -> Point
rot 0 p = p
rot k (x, y) = rot (k - 90) (y, -x)

step :: Ship -> Inst -> Ship
step (pos, ang) (inst, n)
  | inst `elem` "NESW" = (pos +. dir inst *: n, ang)
  | otherwise = case inst of
    'L' -> turn (-n)
    'R' -> turn n
    'F' -> forward $ \case 
      0 -> (-n, 0)
      90 -> (0, n)
      180 -> (n, 0)
      270 -> (0, -n)
  where
    turn d = (pos, (ang + d) `mod` 360)
    forward f = (pos +. f ang, ang)

step' :: Ship' -> Inst -> Ship'
step' (pos, ang, waypoint) (inst, n)
  | inst `elem` "NESW" = (pos, ang, waypoint +. dir inst *: n)
  | otherwise = case inst of
    'L' -> (pos, ang, rot ((-n) `mod` 360) waypoint)
    'R' -> (pos, ang, rot n waypoint)
    'F' -> (pos +. waypoint *: n, ang, waypoint)

parse :: String -> Inst
parse (k:xs) = (k, read xs)
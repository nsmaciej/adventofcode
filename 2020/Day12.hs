{-# LANGUAGE LambdaCase #-}
import Advent

type Dir = (Char, Int)
type Point = (Int, Int)
type Ship = (Point, Int)

main = runSoln' (map parse . lines) part1 (const 0)

part1 :: [Dir] -> Int
part1 = dist . fst . foldl' step ((0, 0), 90)
  where
    dist (x, y) = abs x + abs y

(+.) :: Point -> Point -> Point
(x, y) +. (x', y') = (x + x', y + y')

step :: Ship -> Dir -> Ship
step (pos, ang) (inst, n) = case inst of
  'N' -> move (-n, 0)
  'S' -> move (n, 0)
  'E' -> move (0, n)
  'W' -> move (0, -n)
  'L' -> turn (-n)
  'R' -> turn n
  'F' -> forward $ \case 
    0 -> (-n, 0)
    90 -> (0, n)
    180 -> (n, 0)
    270 -> (0, -n)
  where
    move d = (pos +. d, ang)
    turn d = (pos, (ang + d) `mod` 360)
    forward f = (pos +. f ang, ang)

parse :: String -> Dir
parse (k:xs) = (k, read xs)
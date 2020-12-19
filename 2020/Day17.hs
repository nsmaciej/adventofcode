{-# LANGUAGE BangPatterns #-}
import Advent (runSoln')
import Data.Set (Set)
import qualified Data.Set as S

data Point = P !Int !Int !Int !Int deriving (Show, Eq, Ord)
type World = Set Point
type Around = Point -> [Point]

main = runSoln' world (solve False) (solve True)

solve :: Bool -> World -> Int
solve p2 w = S.size $ iterate (step p2) w !! 6

step :: Bool -> World -> World
step p2 w = S.union kept new
  where
    kept = S.filter ((`elem` [3,4]) . neighbors) w
    new = S.filter ((== 3) . neighbors) . S.fromList $ concatMap (around p2) w
    neighbors = length . filter (`S.member` w) . (around p2)

around :: Bool -> Point -> [Point]
around vary_w (P w' x' y' z') = [P (w' + w) (x' + x) (y' + y) (z' + z) |
  w <- if vary_w then [0] else [-1..1],
  x <- [-1..1],
  y <- [-1..1],
  z <- [-1..1]
  ]

world :: String -> World
world str = S.fromList [P 0 0 y x | (y, xs) <- zip [0..] (lines str), (x, c) <- zip [0..] xs, c == '#']
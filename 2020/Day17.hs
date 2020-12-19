{-# LANGUAGE BangPatterns #-}
import Advent (runSoln')
import Control.Monad (liftM2)
import Data.Set (Set)
import qualified Data.Set as S

type Point = [Int]
type World = Set Point
type Around = Point -> [Point]

main = runSoln' world (solve 3) (solve 4 . S.map (0 :))

solve :: Int -> World -> Int
solve d w = S.size $ iterate (step d) w !! 6

step :: Int -> World -> World
step d w = S.union kept new
  where
    kept = S.filter ((`elem` [3,4]) . neighbors) w
    new = S.fromList . filter ((== 3) . neighbors) $ concatMap (around d) w
    neighbors = length . filter (`S.member` w) . (around d)

around :: Int -> Point -> [Point]
around d p = map (zipWith (+) p) $ iterate (liftM2 (:) [-1..1]) [[]] !! d

world :: String -> World
world str = S.fromList [[0, y, x] | (y, xs) <- zip [0..] (lines str), (x, c) <- zip [0..] xs, c == '#']
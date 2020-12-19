import Advent (runSoln')
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (replicateM)

data Point = P !Int !Int !Int !Int deriving (Show, Eq, Ord)
data VaryW = ZeroW | VaryW deriving (Show, Eq)
type World = Set Point

main = runSoln' world (solve ZeroW) (solve VaryW)

solve :: VaryW -> World -> Int
solve p2 w = S.size $ iterate (step p2) w !! 6

step :: VaryW -> World -> World
step p2 w = S.union kept new
  where
    kept = S.filter ((`elem` [3,4]) . neighbours) w
    new = S.filter ((== 3) . neighbours) . S.fromList $ concatMap (around p2) w
    neighbours = length . filter (`S.member` w) . (around p2)

around :: VaryW -> Point -> [Point]
around vary_w (P w' x' y' z') = [
    P (w' + w) (x' + x) (y' + y) (z' + z) |
    w <- if vary_w == ZeroW then [0] else [-1..1],
    [x, y, z] <- replicateM 3 [-1..1]
  ]

world :: String -> World
world str = S.fromList [P 0 0 y x | (y, xs) <- zip [0..] (lines str), (x, c) <- zip [0..] xs, c == '#']
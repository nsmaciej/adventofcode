import Advent
import Data.Set (Set)
import qualified Data.Set as S

main = runSoln' init part1 part2

part1 :: String -> Int
part1 = S.size . fly

part2 :: String -> Int
part2 xs = S.size $ fly l `S.union` fly r where (l, r) = uninterleave xs

uninterleave :: [a] -> ([a], [a])
uninterleave (x : y : r) = (x : xs, y : ys) where (xs, ys) = uninterleave r
uninterleave x = (x, [])

fly :: String -> Set (Pt Int)
fly = snd . foldl (\(p, v) m -> (p +> m, S.insert (p +> m) v)) (zero, S.singleton zero)

(+>) :: (Pt Int) -> Char -> (Pt Int)
p +> c = p + fromPair (dir c)
  where
    dir '<' = (0, -1)
    dir '>' = (0, 1)
    dir '^' = (-1, 0)
    dir 'v' = (1, 0)
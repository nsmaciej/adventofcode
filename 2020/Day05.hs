import Advent

main = runSoln' (map parse . lines) maximum part2

part2 :: [Int] -> Int
part2 xs = sum [minimum xs .. maximum xs] - sum xs

parse :: String -> Int
parse x = 8 * locate 'B' r + locate 'R' c
  where
    (r, c) = span (`elem` "FB") x

-- Approach stolen from u/Psy_Blades
locate :: Char -> String -> Int
locate hc = foldl' (\a x -> a * 2 + if x == hc then 1 else 0) 0

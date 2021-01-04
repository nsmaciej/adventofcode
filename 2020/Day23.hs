import Advent
import Data.Sequence (Seq(Empty), (><), (|>))
import qualified Data.Sequence as S
import Data.Foldable (toList)

main :: IO ()
main = runSoln' (S.fromList . map (read . pure)) (Print . part1) (const 0)

part1 :: Seq Int -> String
part1 = concat . toList . fmap show . (!! 100) . iterate step

step :: Seq Int -> Seq Int
step cups = (l >< pick >< r) |> current
  where
    current = S.index cups 0
    (pick, left) = S.splitAt 3 $ S.drop 1 cups
    Just dest = findMaxIx (< current) left <|> findMaxIx (const True) left
    (l, r) = S.splitAt (dest + 1) left

findMaxIx :: Ord a => (a -> Bool) -> Seq a -> Maybe Int
findMaxIx _ Empty = Nothing
findMaxIx f xs = fst <$> S.foldlWithIndex go Nothing xs
  where
    go Nothing i x = if f x then Just (i, x) else Nothing
    go (Just (ti, tx)) i x = if f x && x > tx then Just (i, x) else Just (ti, tx)
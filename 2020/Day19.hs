import Advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Rule = Lit Char | Or Rule Rule | And Rule Rule | See Int deriving (Show, Eq, Ord)
type Input = (Map Int Rule, [String])

main = runSoln' (parseAll pInput) part1 part2

part1 :: Input -> Int
part1 (rules, xs) = countp ((> 0) . countp null . solve rules (See 0)) xs

part2 :: Input -> Int
part2 (rules, xs) = part1 (add "8: 42 | 42 8" $ add "11: 42 31 | 42 11 31" rules, xs)
  where
    add = uncurry M.insert . parseAll pPair

solve :: Map Int Rule -> Rule -> String -> [String]
solve _ (Lit e) (x:xs) = if e == x then [xs] else []
solve _ (Lit _) [] = []
solve m (Or l r) xs = solve m l xs ++ solve m r xs
solve m (And l r) xs = concatMap (solve m r) $ solve m l xs
solve m (See n) xs = solve m (m M.! n) xs

l :: Parser a -> Parser a
l p = p <* hspace

pInput :: Parser Input
pInput = (,) <$> (M.fromList <$> pPair `endBy` newline)
  <* newline
  <*> some letterChar `endBy` newline
  <* eof

pPair :: Parser (Int, Rule)
pPair = (,) <$> l decimal <* l (char ':') <*> pRule

pRule :: Parser Rule
pRule = Lit <$> (try (char '"') *> letterChar <* l (char '"'))
  <|> foldl1' Or <$> (foldl1' And . map See <$> decimal `endBy` hspace) `sepBy` l (string "|")
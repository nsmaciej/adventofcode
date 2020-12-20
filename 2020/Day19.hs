import Advent
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Foldable

data Rule = Lit Char | Rules [[Int]] deriving (Show, Eq, Ord)
type RuleMap = Map Int Rule
type Input = (RuleMap, [String])

main = runSoln' (parseAll pInput) part1 (const 0)

part1 :: Input -> Int
part1 (rules, xs) = length $ filter (\x -> case matches rules 0 x of
    Just "" -> True
    _ -> False
  ) xs

matches :: RuleMap -> Int -> String -> Maybe String
matches m r xs = case m M.! r of
  Lit e -> case xs of
    (x:xs) -> if e == x then Just xs else Nothing
    _ -> Nothing
  Rules opts -> asum $ map (\bs -> matchesSeq m bs xs) opts

matchesSeq :: RuleMap -> [Int] -> String -> Maybe String
matchesSeq m rs xs = foldlM (\xs r -> matches m r xs) xs rs

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
  <|> Rules <$> (decimal `endBy` hspace) `sepBy` l (string "|")
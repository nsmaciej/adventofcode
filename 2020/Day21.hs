{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Food = (Set String, [String])

main = runSoln' (parseLines pFood) part1 (Print . intercalate "," . solve)

part1 :: [Food] -> Int
part1 foods = sum . map (S.size . (S.\\ bad)) $ map fst foods
  where
    bad = S.fromList $ solve foods

solve :: [Food] -> [String]
solve foods = map fst . sortOn snd $ go queue S.empty
  where
    queue = sortOn (S.size . snd) . M.toList $ translations foods
    go [] _ = []
    go ((allergen, options):xs) used =
      let use = S.findMin $ options S.\\ used
      in (use, allergen) : go xs (S.insert use used)

translations :: [Food] -> Map String (Set String)
translations foods = M.unionsWith S.intersection [M.singleton a is | (is, as) <- foods, a <- as]

pFood :: Parser Food
pFood = (,)
  <$> (S.fromList <$> some letterChar `endBy` hspace)
  <*> ("(contains " *> some letterChar `sepBy` ", " <* ")" <|> pure [])
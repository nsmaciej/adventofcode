{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Maybe
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Food = Food { ingredients :: Set String, allergens :: [String] }
  deriving (Show, Eq)

main = runSoln' (catMaybes . parseLines pFood) part1 part2

part1 :: [Food] -> Int
part1 foods = sum $ map (\Food{..} -> S.size $ ingredients S.\\ allAllergns) foods
  where
    allAllergns = S.unions . M.elems $ translations foods

part2 :: [Food] -> String
part2 foods = intercalate "," . map fst . sortOn snd $ go queue S.empty
  where
    queue = sortOn (S.size . snd) . M.toList $ translations foods
    go [] _ = []
    go ((allergen, options):xs) used =
      let use = S.findMin $ options S.\\ used
      in (use, allergen) : go xs (S.insert use used)

translations :: [Food] -> Map String (Set String)
translations foods = M.unionsWith S.intersection [M.fromList $ map (,ingredients) allergens | Food{..} <- foods]

pFood :: Parser (Maybe Food)
pFood = (\x y -> Food x <$> y)
  <$> (S.fromList <$> some letterChar `endBy` hspace)
  <*> (optional $ "(contains " *> some letterChar `sepBy` ", " <* char ')')
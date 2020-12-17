{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Set (Set)
import qualified Data.Set as Set

data Rule = Rule { name :: String, lo, hi, lo', hi' :: Int } deriving (Show, Eq, Ord)
type Ticket = [Int]
data Input = Input {
  rules :: [Rule],
  your :: Ticket,
  nearby :: [Ticket]
  } deriving (Show)

main = runSoln' (parseAll parse) part1 part2

parse :: Parser Input
parse = Input
  <$> pRule `endBy` newline
  <* "\nyour ticket:\n" <*> pTicket
  <* "\n\nnearby tickets:\n" <*> pTicket `endBy` newline

pRule :: Parser Rule
pRule = Rule
  <$> try (some (anySingleBut ':') <* ": ")
  <*> decimal <* char '-' <*> decimal
  <* " or "
  <*> decimal <* char '-' <*> decimal

pTicket :: Parser Ticket
pTicket = decimal `sepBy` char ','

checkRule :: Int -> Rule -> Bool
checkRule n Rule{..} = n >= lo && n <= hi || n >= lo' && n <= hi'

part1 :: Input -> Int
part1 Input{..} = sum . filter (not . validField rules) . concat $ nearby

validField :: [Rule] -> Int -> Bool
validField rs n = any (checkRule n) rs

vaildRules :: [Rule] -> Ticket -> [Set Rule]
vaildRules rs t = map (\f -> Set.fromList $ filter (checkRule f) rs) t

possibleRules :: [Rule] -> [Ticket] -> [Set Rule]
possibleRules rs = foldl1' (zipWith Set.intersection) . map (vaildRules rs)

solvePossible :: [Set Rule] -> [(Int, Rule)]
solvePossible = go Set.empty . sortOn (Set.size . snd) . zip [0..]
  where
    go :: Set Rule -> [(Int, Set Rule)] -> [(Int, Rule)]
    go _ [] = []
    go seen (r:rs) =
      let r' = Set.findMin (snd r Set.\\ seen)
      in (fst r, r') : go (Set.insert r' seen) rs

part2 :: Input -> Int
part2 Input{..} = product
  . map ((your !!) . fst)
  . filter (isPrefixOf "departure" . name . snd)
  . solvePossible
  . possibleRules rules
  . filter (all $ validField rules)
  $ nearby
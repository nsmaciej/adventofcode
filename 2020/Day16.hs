import Advent
import Data.Set (Set)
import qualified Data.Set as Set

data Rule = Rule String Int Int Int Int deriving (Show, Ord, Eq)
type Ticket = [Int]
data Input = Input {
  rules :: [Rule],
  your :: Ticket,
  nearby :: [Ticket]
  } deriving (Show)

main = runSoln' (parseAll parse) part1 part2

parse :: Parser Input
parse = Input
  <$> parseRule `endBy` newline
  <* string "\nyour ticket:\n"
  <*> parseTicket
  <* string "\n\nnearby tickets:\n"
  <*> parseTicket `endBy` newline

parseRule :: Parser Rule
parseRule = Rule
  <$> try (some (anySingleBut ':') <* string ": ")
  <*> decimal <* char '-' <*> decimal
  <* string " or "
  <*> decimal <* char '-' <*> decimal

parseTicket :: Parser Ticket
parseTicket = decimal `sepBy` char ','

checkRule :: Int -> Rule -> Bool
checkRule n (Rule _ l h l' h') = (n >= l && n <= h) || (n >= l' && n <= h')

ruleName :: Rule -> String
ruleName (Rule name _ _ _ _) = name

part1 :: Input -> Int
part1 x = sum . filter (not . validField (rules x)) . concat $ nearby x

validTicket :: [Rule] -> Ticket -> Bool
validTicket rs t = all (validField rs) t

validField :: [Rule] -> Int -> Bool
validField rs n = any (checkRule n) rs

vaildRules :: [Rule] -> Ticket -> [Set Rule]
vaildRules rs t = map (\f -> Set.fromList $ filter (checkRule f) rs) t

possibleRules :: [Rule] -> [Ticket] -> [Set Rule]
possibleRules rs = foldl1' (zipWith Set.intersection) . map (vaildRules rs)

solvePossible :: [Set Rule] -> [(Int, Rule)]
solvePossible pos = case findIndex ((1 ==) . length) pos of
  Just i ->
    let rule = Set.findMin (pos !! i)
    in (i, rule) : solvePossible (map (Set.delete rule) pos)
  Nothing -> []

part2 :: Input -> Int
part2 x = product
  . map ((your x !!) . fst)
  . filter (isPrefixOf "departure" . ruleName . snd)
  . solvePossible
  . possibleRules (rules x)
  . filter (validTicket (rules x))
  $ nearby x
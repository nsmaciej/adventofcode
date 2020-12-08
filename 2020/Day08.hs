import Advent
import qualified Data.Set as Set

data Op = Nop Int | Jmp Int | Acc Int deriving (Show, Eq)
type Cpu = (Int, Int)

main = runSoln' (parseLines pOp) part1 part2

run :: Cpu -> [Op] -> [Cpu]
run c@(ip, _) ops
  | ip >= length ops = []
  | otherwise = run (step c $ ops !! ip) ops

part1 :: [Op] -> Int
part1 ops = snd $ states !! (i - 1)
  where
    states = run (0, 0) ops
    Just i = dupIx $ map fst states

halt :: [Op] -> Maybe Int
halt ops = case dupIx $ map fst states of
    Nothing -> Just . snd $ last states
    Just _ -> Nothing
  where
    states = run (0, 0) ops

update :: Int -> (a -> a) -> [a] -> [a]
update _ _ [] = []
update n f (x:xs)
  | n == 0 = f x : xs
  | otherwise = x : update (n - 1) f xs

part2 :: [Op] -> Int
part2 ops = head [a | Just a <- map (\i -> halt (update i flipOp ops)) [0..]]

dupIx :: Ord a => [a] -> Maybe Int
dupIx xs = dup' xs 0 Set.empty
  where
    dup' (x:xs) i s = if Set.member x s then Just i else dup' xs (i + 1) (Set.insert x s)
    dup' [] _ _ = Nothing

step :: Cpu -> Op -> Cpu
step (p, x) op = case op of
  (Acc n) -> (p + 1, x + n)
  (Jmp n) -> (p + n, x)
  (Nop _) -> (p + 1, x)

flipOp :: Op -> Op
flipOp (Nop n) = (Jmp n)
flipOp (Jmp n) = (Nop n)
flipOp x = x

pOp :: Parser Op
pOp = choice [Nop <$ string' "nop", Jmp <$ string' "jmp", Acc <$ string' "acc"] <* hspace1 <*> pOffset

-- negate inspired by incertia.
pOffset :: Parser Int
pOffset = (id <$ char '+' <|> negate <$ char '-') <*> decimal
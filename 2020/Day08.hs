{-# LANGUAGE BangPatterns #-}
import Advent
import Data.Maybe
import qualified Data.Set as Set
import Control.Applicative

data Op = Nop Int | Jmp Int | Acc Int deriving (Show, Eq)
type Cpu = (Int, Int)

main = runSoln' (parseLines pOp) part1 part2

run :: [Op] -> [Cpu]
run ops = run' (0, 0)
  where
    run' (p, x) =
      let (!p', !x') = step (p, x) (ops !! p)
      in if p' >= length ops then [(p', x')] else (p', x') : run' (p', x')

part1 :: [Op] -> Int
part1 ops = snd $ ss !! (i - 1)
  where
    ss = run ops
    Just i = dup $ map fst ss

halt :: [Op] -> Maybe Int
halt ops = case dup $ map fst ss of
    Nothing -> Just . snd $ last ss
    Just _ -> Nothing
  where
    ss = run ops

update :: Int -> (a -> a) -> [a] -> [a]
update _ _ [] = []
update n f (x:xs)
  | n == 0 = f x : xs
  | otherwise = x : update (n - 1) f xs

part2 :: [Op] -> Int
part2 ops = head . catMaybes $ map (\i -> halt $ update i flipOp ops) [0..length ops]

dup :: Ord a => [a] -> Maybe Int
dup xs = dup' xs 0 Set.empty
  where
    dup' (x:xs) i s = if Set.member x s then Just i else dup' xs (i + 1) (Set.insert x s)
    dup' [] _ _ = Nothing

step :: Cpu -> Op -> Cpu
step (p, x) (Acc n) = (p + 1, x + n)
step (p, x) (Jmp n) = (p + n, x)
step (p, x) (Nop _) = (p + 1, x)

flipOp :: Op -> Op
flipOp (Nop n) = (Jmp n)
flipOp (Jmp n) = (Nop n)
flipOp x = x

pOp :: Parser Op
pOp = choice [Nop <$ string "nop", Jmp <$ string "jmp", Acc <$ string "acc"] <* hspace1 <*> pOffset

pOffset :: Parser Int
pOffset = liftA2 (\s n -> if s == '+' then n else -n) (char '+' <|> char '-') decimal
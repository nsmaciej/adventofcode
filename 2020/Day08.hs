{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Sequence (Seq)
import qualified Data.IntSet as Set
import qualified Data.Sequence as Seq

data Op = Nop Int | Jmp Int | Acc Int deriving (Show, Eq)
type Cpu = (Int, Int)

main = runSoln' (Seq.fromList . parseLines pOp) part1 part2

run :: Cpu -> Seq Op -> [Cpu]
run c@(ip, _) ops = case ops Seq.!? ip of
  Nothing -> [c]
  Just op -> c : run (step c op) ops

part1 :: Seq Op -> Int
part1 ops = snd $ states !! (i - 1)
  where
    states = run (0, 0) ops
    Just i = dupIx $ map fst states

halt :: Seq Op -> Maybe Int
halt ops = case dupIx $ map fst states of
    Nothing -> Just . snd $ last states
    Just _ -> Nothing
  where
    states = run (0, 0) ops

part2 :: Seq Op -> Int
part2 ops = head [a | Just a <- map (\i -> halt (Seq.adjust flipOp i ops)) [0..]]

dupIx :: [Int] -> Maybe Int
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
pOp = choice [Nop <$  "nop", Jmp <$ "jmp", Acc <$ "acc"]
  <* hspace1
  <*> signed hspace decimal
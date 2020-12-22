{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Foldable (toList)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S

type Game = (Seq Int, Seq Int)

main = runSoln' (parseAll pGame) (uncurry play) (const 0)

play :: Seq Int -> Seq Int -> Int
play Empty rs = score rs
play ls Empty = score ls
play (l :<| ls) (r :<| rs)
  | l > r = play (ls |> l |> r) rs
  | r > l = play ls (rs |> r |> l)

score :: Seq Int -> Int
score deck = sum $ zipWith (*) [1..] (reverse $ toList deck)

pGame :: Parser Game
pGame = (\x y -> (S.fromList x, S.fromList y))
  <$> ("Player 1:\n" *> decimal `endBy` newline <* newline)
  <*> ("Player 2:\n" *> decimal `endBy` newline)
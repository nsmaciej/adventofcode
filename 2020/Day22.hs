{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Deck = Seq Int
type Game = (Deck, Deck)
type Key = (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Int)

main = runSoln'
  (parseAll pGame)
  (uncurry play) 
  (either score score . uncurry (playRec Set.empty))

play :: Deck -> Deck -> Int
play Empty rs = score rs
play ls Empty = score ls
play (l :<| ls) (r :<| rs)
  | l > r = play (ls |> l |> r) rs
  | r > l = play ls (rs |> r |> l)

playRec :: Set Key -> Deck -> Deck -> Either Deck Deck 
playRec _ Empty rs = Right rs
playRec _ ls Empty = Left ls
playRec seen lall@(l :<| ls) rall@(r :<| rs)
  | Set.member (encode lall rall) seen = Left ls
  | l <= Seq.length ls && r <= Seq.length rs =
    case playRec Set.empty (Seq.take l ls) (Seq.take r rs) of
      Left _ -> left
      Right _ -> right
  | l > r = left
  | r > l = right
  where
    seen' = Set.insert (encode lall rall) seen
    left = playRec seen' (ls |> l |> r) rs
    right = playRec seen' ls (rs |> r |> l)

encode :: Deck -> Deck -> Key
encode ls rs = (headMay ls, lastMay ls, headMay rs, lastMay rs, Seq.length ls)

score :: Seq Int -> Int
score deck = sum $ zipWith (*) [1..] (reverse $ toList deck)

pGame :: Parser (Deck, Deck)
pGame = (\x y -> (Seq.fromList x, Seq.fromList y))
  <$> ("Player 1:\n" *> decimal `endBy` newline <* newline)
  <*> ("Player 2:\n" *> decimal `endBy` newline)
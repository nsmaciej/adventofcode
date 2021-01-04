{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Advent (runSoln', Print(..))
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Unboxed vector representing a linked list.
type Cups = V.Vector Int
-- | Current game state: the 'load'ed input and the value of the current cup.
data Game = Game Int Cups deriving (Show)

main :: IO ()
main = runSoln' (map (read . pure)) (Print . part1) part2

part1 :: [Int] -> String
part1 = concat . map show . tail . unload1 . (!! 100) . iterate step . load

part2 :: [Int] -> Int
part2 game = soln !! 1 * soln !! 2
  where
    soln = unload1 . (!! 10_000_000) . iterate step . load $ game ++ [10..1000_000]

-- | Load the input into a list where each index is the element stored and the
-- value is the index of the next element.
load :: [Int] -> Game
load xs = Game (head xs) $ V.replicate (1 + length xs) 0 V.// go (xs ++ [head xs])
  where
    go (x : y : xs) = (x, y) : go (y : xs)
    go _ = []

unload :: Game -> [Int]
unload (Game current list) = go (V.length list) current
  where
    go 1 _ = []
    go n k = k : go (n - 1) (list V.! k)

-- | Unload, staring the cups at cup '1'.
unload1 :: Game -> [Int]
unload1 (Game _ list) = unload $ Game 1 list

-- | Execute a single step of the game.
step :: Game -> Game
step (Game current list) = runST $ do
  let next = (list V.!)
  let pick@[!c1, _, !c3] = take 3 . tail $ iterate next current
  let !dest = predCup (V.length list) pick current
  let !dest' = next dest
  let !c3' = next c3
  mv <- V.unsafeThaw list
  MV.unsafeWrite mv dest c1
  MV.unsafeWrite mv c3 dest'
  MV.unsafeWrite mv current c3'
  list' <- V.unsafeFreeze mv
  return $ Game (list' V.! current) list'

-- | Find the index of the previous unused cup.
predCup :: Int -> [Int] -> Int -> Int
predCup len taken n = go n
  where
    go n
      | n > 1 = if n - 1 `elem` taken then go (n - 1) else n - 1
      | otherwise = go len
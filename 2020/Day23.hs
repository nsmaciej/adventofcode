{-# LANGUAGE NumericUnderscores #-}
module Main where

import Advent (runSoln', Print(..))
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Mutable version of 'Game' over a PrimState.
data MGame s = MGame Int (V.MVector s Int)

main :: IO ()
main = runSoln' (map (read . pure)) (Print . part1) part2

part1 :: [Int] -> String
part1 = concat . map show . tail . play 100

part2 :: [Int] -> Int
part2 game = soln !! 1 * soln !! 2
  where
    soln = play 10_000_000 $ game ++ [10..1000_000]

-- | Load the input into a list where each index is the element stored and the
-- value is the index of the next element.
load :: [Int] -> V.Vector Int
load xs = V.replicate (1 + length xs) 0 V.// go (xs ++ [head xs])
  where
    go (x : y : xs) = (x, y) : go (y : xs)
    go _ = []

-- | Convert a 'Game' into the cup list, starting with the cup number one.
unload :: V.Vector Int -> [Int]
unload xs = go (V.length xs) 1
  where
    go 1 _ = []
    go n k = k : go (n - 1) (xs V.! k)

play :: Int -> [Int] -> [Int]
play n xs = unload $ runST $ do
  mv <- V.thaw $ load xs
  MGame _ mv' <- iterateM step (MGame (head xs) mv) !! (n + 1)
  V.freeze mv'

-- | Execute a single step of the game.
step :: MGame s -> ST s (MGame s)
step (MGame current cups) = do
  c1 <- next current
  c2 <- next c1
  c3 <- next c2
  c3_next <- next c3
  let dest = predCup (MV.length cups) (c2,c3,c3) current
  dest_next <- next dest
  write dest c1
  write c3 dest_next
  write current c3_next
  MGame <$> next current <*> pure cups
  where
    next = MV.read cups
    write = MV.write cups

-- | Find the index of the previous unused cup.
predCup :: Int -> (Int, Int, Int) -> Int -> Int
predCup len (c1,c2,c3) n = go n
  where
    go n
      | n > 1 = if n - 1 == c1 || n - 1 == c2 || n - 1 == c3 then go (n - 1) else n - 1
      | otherwise = go len

iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f = iterate (>>= f) . return
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
import Advent (runSoln', Print(..))
import Data.Int (Int32)
import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as MV

type Cups s = MV.MVector s Int32

main :: IO ()
main = runSoln' (map (read . pure)) (Print . part1) part2

part1 :: [Int32] -> String
part1 = concat . map show . tail . play 100

part2 :: [Int32] -> Integer
part2 game = toInteger x * toInteger y
  where
    (_:x:y:_) = play 10_000_000 $ game ++ [10..1000_000]

load :: [Int32] -> ST s (Cups s)
load xs = do
  v <- MV.new (1 + length xs)
  zipWithM_ (MV.write v . fromIntegral) xs (tail xs) -- Very neat, copied from glguy.
  MV.write v (fromIntegral $ last xs) (head xs) -- Make it circular.
  return v

unload :: Cups s -> ST s [Int32]
unload xs = go (MV.length xs) 1
  where
    go 1 _ = return []
    go n k = (k :) <$> (MV.read xs (fromIntegral k) >>= go (n - 1))

play :: Int32 -> [Int32] -> [Int32]
play n xs = runST $ snd <$> go (n - 1) >>= unload
  where
    go 0 = (head xs,) <$> load xs
    go n = go (n - 1) >>= step

-- | Execute a single step of the game.
step :: (Int32, Cups s) -> ST s (Int32, Cups s)
step (current, cups) = do
  c1 <- next current
  c2 <- next c1
  c3 <- next c2
  let dest = until (\x -> x /= c1 && x /= c2 && x /= c3) dec (current - 1)
  c3' <- next c3
  dest' <- next dest
  write dest c1
  write c3 dest'
  write current c3'
  (,cups) <$> next current
  where
    next = MV.read cups . fromIntegral
    write = MV.write cups . fromIntegral
    dec 1 = fromIntegral $ MV.length cups - 1
    dec k = k - 1
module Advent (Pt, fromPair, zero, runSoln, runSoln', countp) where

import Data.Char
import System.Environment (getArgs, getProgName)

runSoln :: (String -> IO ()) -> (String -> IO ()) -> IO ()
runSoln f g = do
  args <- getArgs
  day <- filter isDigit <$> getProgName
  let file = case args of
        ["-"] -> "/dev/stdin"
        [m] -> m
        _ -> "inputs/day" ++ day ++ ".txt"
  input <-  readFile file
  f input
  g input

runSoln' :: (Show l, Show r) => (String -> a) -> (a -> l) -> (a -> r) -> IO ()
runSoln' p f g = runSoln (print . f . p) (print . g . p)
data Pt a = Pt a a deriving (Eq, Ord, Show)

instance Num a => Num (Pt a) where
  (Pt x y) + (Pt x' y') = Pt (x + x') (y + y')
  (Pt x y) * (Pt x' y') = Pt (x * x') (y * y')
  negate (Pt x y) = Pt (- x) (- y)
  abs (Pt x y) = Pt (abs x) (abs y)
  signum (Pt x y) = Pt (signum x) (signum y)
  fromInteger n = Pt (fromInteger n) (fromInteger n)

fromPair :: Num a => (a, a) -> Pt a
fromPair (x, y) = Pt x y

zero :: Num a => Pt a
zero = Pt 0 0

countp :: (a -> Bool) -> [a] -> Int
countp = (length .) . filter
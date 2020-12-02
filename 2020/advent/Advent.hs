module Advent (runSoln, runSoln') where

import Data.Char
import System.Environment

runSoln :: (String -> IO ()) -> (String -> IO ()) -> IO ()
runSoln f g = do
  day <- filter isDigit <$> getProgName
  input <- readFile $ "inputs/day" ++ day ++ ".txt"
  f input
  g input

runSoln' :: (Show l, Show r) => (String -> a) -> (a -> l) -> (a -> r) -> IO ()
runSoln' p f g = runSoln (print . f . p) (print . g . p)
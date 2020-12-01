module Soln (runSoln, runSoln') where

import Data.Char
import System.Environment

runSoln :: (String -> IO ()) -> (String -> IO ()) -> IO ()
runSoln f g = do
  day <- filter isDigit <$> getProgName
  input <- readFile $ "inputs/day" ++ day ++ ".txt"
  f input
  g input

runSoln' :: (String -> a) -> (a -> IO ()) -> (a -> IO ()) -> IO ()
runSoln' p f g = runSoln (f . p) (g . p)
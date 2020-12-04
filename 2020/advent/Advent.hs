module Advent (Parser, runSoln, runSoln', parseString, countp) where

import Data.Char
import System.Environment
import Data.Void
import Text.Megaparsec

inputFilePath :: IO FilePath
inputFilePath = do
  day <- filter isDigit <$> getProgName
  return $ "inputs/day" ++ day ++ ".txt"

runSoln :: (String -> IO ()) -> (String -> IO ()) -> IO ()
runSoln f g = do
  input <- readFile =<< inputFilePath
  f input
  g input

countp :: (a -> Bool) -> [a] -> Int
countp = (length .) . filter

runSoln' :: (Show l, Show r) => (String -> a) -> (a -> l) -> (a -> r) -> IO ()
runSoln' p f g = runSoln (print . f . p) (print . g . p)

type Parser = Parsec Void String

parseString :: Parser a -> String -> a
parseString parser = either (error . errorBundlePretty) id . runParser parser "input"
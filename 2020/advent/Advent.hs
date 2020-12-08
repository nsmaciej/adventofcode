module Advent (
  Parser, runSoln, runSoln', parseAll, parseLines, countp,
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Char.Lexer,
  module Data.List,
  module Data.Char,
) where

import Data.Char
import Data.List
import System.Environment
import System.CPUTime
import Data.Void
import Text.Printf
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char, letterChar, lowerChar, spaceChar, string, newline, hspace, hspace1)
import Text.Megaparsec.Char.Lexer (decimal)

inputFilePath :: IO FilePath
inputFilePath = do
  day <- filter isDigit <$> getProgName
  return $ "inputs/day" ++ day ++ ".txt"

runSoln :: (String -> IO ()) -> (String -> IO ()) -> IO ()
runSoln f g = do
  input <- readFile =<< inputFilePath
  start <- getCPUTime
  f input
  g input
  end <- getCPUTime
  printf "Took %.3fs\n" (fromInteger (end - start) * 1e-12 :: Double)

countp :: (a -> Bool) -> [a] -> Int
countp = (length .) . filter

runSoln' :: (Show l, Show r) => (String -> a) -> (a -> l) -> (a -> r) -> IO ()
runSoln' p f g = runSoln (print . f . p) (print . g . p)

type Parser = Parsec Void String

parseAll :: Parser a -> String -> a
parseAll parser = either (error . errorBundlePretty) id . runParser parser "input"

parseLines :: Parser a -> String -> [a]
parseLines parser = map (parseAll parser) . lines
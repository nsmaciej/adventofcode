{-# LANGUAGE BangPatterns #-}
module Advent (
  Parser, runSoln, runSoln', parseAll, parseLines, countp,
  (.+.),
  Print(Print),
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
import Text.Megaparsec.Char (char, letterChar, lowerChar, spaceChar, string, string', newline, hspace, hspace1)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Print = Print { unPrint :: String }

instance Show Print where
  show = unPrint

inputFilePath :: IO FilePath
inputFilePath = do
  args <- getArgs
  day <- filter isDigit <$> getProgName
  return $ case args of
    ["-"] -> "/dev/stdin"
    [path] -> path
    [] -> "inputs/day" ++ day ++ ".txt"

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
parseAll parser = either (error . errorBundlePretty) id . runParser (parser <* eof) "input"

parseLines :: Parser a -> String -> [a]
parseLines parser = parseAll (parser `sepEndBy1` newline)


infixl 6 .+.
(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(!x, !y) .+. (!x', !y') = (x + x', y + y')

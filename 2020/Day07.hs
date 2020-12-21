{-# LANGUAGE OverloadedStrings #-}
import Advent
import Data.Map (Map)
import qualified Data.Map as M

type Bag = (Int, String)

main = runSoln'
  (M.fromList . parseLines pRule)
  (\bs -> countp (contains bs "shiny gold") $ M.keys bs)
  (depCount "shiny gold")

contains :: Map String [Bag] -> String -> String -> Bool
contains bs t x = elem t cs || any (contains bs t) cs
  where
    cs = map snd $ bs M.! x

depCount :: String -> Map String [Bag] -> Int
depCount t bs = sum . map (\(i, x) -> i + i * depCount x bs) $ bs M.! t

pRule :: Parser (String, [Bag])
pRule = (,)
  <$> pName <* " contain "
  <*> ([] <$ "no other bags" <|> pBag `sepBy1` ", ")
  <* char '.'

pBag :: Parser Bag
pBag = (,) <$> decimal <* char ' ' <*> pName

pName :: Parser String
pName = manyTill (lowerChar <|> char ' ') (" bags" <|> " bag")
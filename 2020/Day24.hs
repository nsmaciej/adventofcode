{-# LANGUAGE OverloadedStrings #-}
import Advent
import qualified Data.Set as S

data Step = E | SE | SW | W | NW | NE deriving (Show, Eq)
type Hexagon = (Int, Int)

main :: IO ()
main = runSoln' blackTiles length part2

blackTiles :: String -> [Hexagon]
blackTiles = map head . filter (odd . length) . group . sort . map identify . parseLines parseSteps

part2 :: [Hexagon] -> Int
part2 = (!! 100) . map S.size . iterate step . S.fromList

step :: S.Set Hexagon -> S.Set Hexagon
step w = S.union keep add
  where
    keep = S.filter ((`elem` [1,2]) . neighbours) w
    add = S.filter ((== 2) . neighbours) . S.fromList . filter (`S.notMember` w) $ concatMap around w
    neighbours = countp (`S.member` w) . around

around :: Hexagon -> [Hexagon]
around h = map ((h .+.) . move) [E, SE, SW, W, NW, NE]

parseSteps :: Parser [Step]
parseSteps = some $ choice
  [E <$ "e", SE <$ "se", SW <$ "sw", W <$ "w", NW <$ "nw", NE <$ "ne"]

identify :: [Step] -> Hexagon
identify = foldl1 (.+.) . map move

move :: Step -> Hexagon
move s = case s of
  E -> (1, 0)
  SE -> (0, 1)
  SW -> (-1, 1)
  W -> (-1, 0)
  NW -> (0, -1)
  NE -> (1, -1)
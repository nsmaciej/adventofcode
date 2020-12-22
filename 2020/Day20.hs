{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
import Advent
import Data.Vector (Vector)
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Tile = Vector (Vector Char)
type Edge = Vector Char

main = runSoln' (parseAll $ pTile `sepEndBy` newline) part1 (const 0)

part1 :: [(Int, Tile)] -> Int
part1 tiles = product
  . map head
  . M.elems
  . M.filter ((==2) . length)
  . groupMap id
  $ findEdges tiles

findEdges :: [(Int, Tile)] -> [Int]
findEdges tiles = map (fst . head)
  . M.elems
  . M.filter ((==1) . length)
  . groupMap snd
  $ concatMap (\(i, t) -> map ((i,) . canonical) $ edges t) tiles

groupMap :: Ord b => (a -> b) -> [a] -> Map b [a]
groupMap key = foldl' (\a x -> M.insertWith (++) (key x) [x] a) M.empty

rotate :: Tile -> Tile
rotate tile = V.generate len (\y -> V.generate len (\x -> go y x))
  where
    len = V.length tile
    go y x = tile ! (V.length tile - 1 - x, y)

canonical :: Edge -> Edge
canonical edge = if edge < V.reverse edge then edge else V.reverse edge

edges :: Tile -> [Edge]
edges tile = [tile !- 0, tile !- (len - 1), tile !| 0, tile !| (len - 1)]
  where
    len = V.length tile

(!-), (!|) :: Tile -> Int -> Edge
(!-) = (V.!)
(!|) tile !x = V.generate (V.length tile) (\y -> tile ! (y, x))

(!) :: Tile -> (Int, Int) -> Char
(!) tile (!y, !x) = tile V.! y V.! x

pTile :: Parser (Int, Tile)
pTile = (,)
  <$> (string "Tile " *> decimal <* string ":\n")
  <*> (V.fromList <$> (V.fromList <$> some (oneOf "#.")) `endBy` newline)

debugTile :: Tile -> IO ()
debugTile = mapM_ (putStrLn . V.toList)
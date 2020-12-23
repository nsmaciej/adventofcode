{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
import Advent
import Data.Vector (Vector)
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Foldable

type Grid = Vector (Vector (Maybe Tile))
type Edge = Vector Char
data Tile = Tile { tileId :: Int, tileVec :: Vector (Vector Char) } deriving (Show, Ord)

instance Eq Tile where
  x == y = tileId x == tileId y

main = runSoln' (parseAll $ pTile `sepEndBy` newline) (product . map (tileId . fst) . findCorners) (part2)

part2 :: [Tile] -> _
part2 tiles = V.map (V.map $ tileId . fromJust) $ solve rest (1, 0) withTop
  where
    (corner, [left, top]) = head $ findCorners tiles
    Just corner' = fit (\t -> t !| 0 ==| left && t !- 0 ==| top) corner
    side = isqrt $ length tiles
    empty = V.replicate side (V.replicate side Nothing)
    (withTop, rest) = solveTop (tiles \\ [corner']) 1 (update empty (0, 0) corner')

solve :: [Tile] -> (Int, Int) -> Grid -> Grid
solve tiles (y, x) grid
  | y >= side = grid
  | x >= side = solve tiles (y + 1, 0) grid
  | (c:_) <- mapMaybe (\i -> fit go i) tiles =
    solve (tiles \\ [c]) (y, x + 1) (update grid (y, x) c)
  where
    go c = fromJust (grid ! (y - 1, x)) !- (-1) == c !- 0
    side = V.length grid

solveTop :: [Tile] -> Int -> Grid -> (Grid, [Tile])
solveTop tiles x grid
  | x >= V.length grid = (grid, tiles)
  | (c:_) <- mapMaybe (\i -> fit go i) tiles =
    solveTop (tiles \\ [c]) (x + 1) (update grid (0, x) c)
  where
    go c = fromJust (grid ! (0, x - 1)) !| (-1) == c !| 0

-- | Returns a rotated and flipped 'Tile' that passes the test or 'Nothing' otherwise.
fit :: (Tile -> Bool) -> Tile -> Maybe Tile
fit f c = actions c <|> actions (flipV c)
  where
    test c = if f c then Just c else Nothing
    actions c = asum . map (\r -> test (r c)) . take 4 $ iterate (rotateR .) id

findCorners :: [Tile] -> [(Tile, [Edge])]
findCorners = filter ((== 2) . length . snd) . findEdges 

findEdges :: [Tile] -> [(Tile, [Edge])]
findEdges tiles = M.toList . M.map (map snd) $ groupMap fst allEdges
  where
    allEdges :: [(Tile, Edge)]
    allEdges = map head
      . filter ((== 1) . length)
      . groupList (canonical . snd)
      $ concatMap (\t -> map (t,) $ edges t) tiles

groupMap :: Ord b => (a -> b) -> [a] -> Map b [a]
groupMap key = foldl' (\a x -> M.insertWith (++) (key x) [x] a) M.empty

groupList :: Ord b => (a -> b) -> [a] -> [[a]]
groupList key = M.elems . groupMap key

-- | Rotates a 'Tile' right.
rotateR :: Tile -> Tile
rotateR = mapTile $ \t -> V.generate (V.length t) (\y -> V.generate (V.length t) (\x -> go t y x))
  where
    go tile y x = tile ! (V.length tile - 1 - x, y)

-- | Flips a 'Tile' vertically.
flipV :: Tile -> Tile
flipV = mapTile (V.map V.reverse)

canonical :: Edge -> Edge
canonical edge = if edge < V.reverse edge then edge else V.reverse edge

-- | Find all the 'Edge's of a 'Tile'.
edges :: Tile -> [Edge]
edges tile = [tile !- 0, tile !- (-1), tile !| 0, tile !| (-1)]

-- | Get a column or a row at a given index, negative indexes index from the end.
(!-), (!|) :: Tile -> Int -> Edge
(!-) Tile{..} !y = tileVec V.! (if y < 0 then V.length tileVec + y else y)
(!|) Tile{..} !x = V.generate (V.length tileVec) (\y -> tileVec ! (y, if x < 0 then V.length tileVec + x else x))

(!) :: Vector (Vector a) -> (Int, Int) -> a
(!) tile (!y, !x) = tile V.! y V.! x

infix 4 ==|
(==|) :: Edge -> Edge -> Bool
(==|) v v' = v == v' || canonical v == canonical v'

update :: Grid -> (Int, Int) -> Tile -> Grid
update grid (y, x) tile = grid V.// [(y, (grid V.! y) V.// [(x, Just tile)])]

pTile :: Parser Tile
pTile = Tile
  <$> (string "Tile " *> decimal <* string ":\n")
  <*> (V.fromList <$> (V.fromList <$> some (oneOf "#.")) `endBy` newline)

-- Debugging functions.

buildTile :: [String] -> Tile
buildTile = Tile (-1) . V.fromList . map V.fromList

debugTile :: Tile -> IO ()
debugTile = mapM_ (putStrLn . V.toList) . tileVec

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

mapTile :: (Vector (Vector Char) -> Vector (Vector Char)) -> Tile -> Tile
mapTile f Tile{..} = Tile tileId (f tileVec)

len :: Tile -> Int
len = V.length . tileVec

-- printGrid :: Int -> Grid -> IO ()
-- printGrid n grid = 
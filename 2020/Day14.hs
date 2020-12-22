import Advent
import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

data Inst = Mask String | Mem Int Int deriving (Show)
type Protocol = String -> Int -> Int -> IntMap Int

main = runSoln' (parseLines pInst) (run v1) (run v2)

run :: Protocol -> [Inst] -> Int
run f = go M.empty ""
  where
    go m _ [] = sum m
    go m _ (Mask mask:xs) = go m mask xs
    go m mask (Mem addr val:xs) = go (M.union (f mask addr val) m) mask xs

v1, v2 :: Protocol
v1 mask addr val = M.singleton addr (apply mask val)
v2 mask addr val = M.fromList $ zip (locations (reverse mask) addr) (repeat val)

locations :: String -> Int -> [Int]
locations [] n = [n]
locations ('0':xs) n = map (\x -> x `shiftL` 1 .|. n .&. 1) $ locations xs (n `shiftR` 1)
locations ('1':xs) n = map (\x -> x `shiftL` 1 .|. 1) $ locations xs (n `shiftR` 1)
locations ('X':xs) n = n' ++ map (.|. 1) n'
  where
    n' = map (`shiftL` 1) $ locations xs (n `shiftR` 1)

apply :: String -> Int -> Int
apply mask n = (n .&. complement (build '0') .|. build '1') .&. (2^36 - 1)
  where
    build bit = foldl' (\a x -> 2 * a + if x == bit then 1 else 0) 0 mask

pInst :: Parser Inst
pInst = Mask <$> (string "mask = " *> some (oneOf "01X"))
  <|> Mem <$> (string "mem[" *> decimal <* string "] = ") <*> decimal
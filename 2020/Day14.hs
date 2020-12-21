import Advent
import Data.Bits
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Inst = Mask String | Mem Int Int deriving (Show)

main = runSoln' (parseLines pInst) (run M.empty "") id

run :: Map Int Int -> String -> [Inst] -> Int
run m _ [] = sum m
run m _ (Mask mask:xs) = run m mask xs
run m mask (Mem addr val:xs) = run (M.insert addr (apply mask val) m) mask xs

apply :: String -> Int -> Int
apply mask n = (n .&. complement (build '0') .|. build '1') .&. (2^36 - 1)
  where
    build bit = foldl' (\a x -> 2 * a + if x == bit then 1 else 0) 0 mask

pInst :: Parser Inst
pInst = Mask <$> (string "mask = " *> some (oneOf "01X"))
  <|> Mem <$> (string "mem[" *> decimal <* string "] = ") <*> decimal
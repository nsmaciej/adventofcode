import Advent
import Data.List
import Data.Maybe
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

main = runSoln' id (solve part1) (solve part2)

part1 :: B.ByteString -> Bool
part1 h = B.head h == 0 && B.index h 1 == 0 && B.index h 2 < 16
part2 :: B.ByteString -> Bool
part2 h = B.head h == 0 && B.index h 1 == 0 && B.index h 2 == 0

solve :: (B.ByteString -> Bool) -> String -> Int
solve p x = fromJust $ find f [0..]
  where
    ctx = MD5.update MD5.init $ C.pack x
    f = p . MD5.finalize . MD5.update ctx . C.pack . show
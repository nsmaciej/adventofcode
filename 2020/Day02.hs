import Advent

type Password = (Int, Int, Char, String)

main = runSoln'
  (map (parseString pPassword) . lines)
  (countp vaild1)
  (countp vaild2)

vaild1 :: Password -> Bool
vaild1 (l, h, c, xs) = x >= l && x <= h
  where
    x = length $ filter (== c) xs

vaild2 :: Password -> Bool
vaild2 (i, j, c, xs) = (xs !! (i - 1) == c) /= (xs !! (j - 1) == c)

pPassword :: Parser Password
pPassword = (,,,)
  <$> decimal <* char '-'
  <*> decimal <* char ' '
  <*> letterChar <* string ": "
  <*> many letterChar
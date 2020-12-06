import Advent

main = runSoln' lines (total delta) (total encode)
  where total f = sum . map ((2 +) . f)

delta :: String -> Int
delta ('\\':'"':xs) = 1 + delta xs
delta ('\\':'\\':xs) = 1 + delta xs
delta ('\\':'x':_:_:xs) = 3 + delta xs
delta (_:xs) = delta xs
delta [] = 0

encode :: String -> Int
encode ('"':xs) = 1 + encode xs
encode ('\\':xs) = 1 + encode xs
encode (_:xs) = encode xs
encode [] = 0
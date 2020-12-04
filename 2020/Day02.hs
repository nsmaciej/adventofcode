import Advent
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String
type Password = (Int, Int, Char, String)

main = runSoln' process (length . filter vaild1) (length . filter vaild2)

vaild1 :: Password -> Bool
vaild1 (l, h, c, xs) = x >= l && x <= h where x = length $ filter (== c) xs

vaild2 :: Password -> Bool
vaild2 (i, i', c, xs) = (xs !! (i -1) == c) /= (xs !! (i' -1) == c)

process :: String -> [Password]
process = map (either (error . errorBundlePretty) id . runParser pPassword "input") . lines

pPassword :: Parser Password
pPassword = (,,,) <$> decimal <* char '-' <*> decimal <* char ' ' <*> letterChar <* string ": " <*> many letterChar
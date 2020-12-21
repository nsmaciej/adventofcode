import Advent
import Control.Applicative ((<**>))

main = runSoln' id (sum . parseLines pExpr) (sum . parseLines pExpr')

sym :: String -> Parser String
sym str = string str <* hspace

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl term op = term <**> rest
  where
    rest = (\op rhs rest lhs -> rest (op lhs rhs)) <$> op <*> term <*> rest <|> pure id

pTerm :: Parser Int -> Parser Int
pTerm expr = decimal <* hspace <|> sym "(" *> expr <* sym ")"

pExpr :: Parser Int
pExpr = chainl (pTerm pExpr) ((+) <$ sym "+" <|> (*) <$ sym "*")

pExpr' :: Parser Int
pExpr' = chainl term ((*) <$ sym "*")
  where
    term = chainl (pTerm pExpr') ((+) <$ sym "+")

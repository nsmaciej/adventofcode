import Advent
import Data.Void
import Data.Bits
import Data.Map (Map)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M

type Parser = Parsec Void String

data Value
  = Wire String | Lit Int
  deriving (Show)

data Op
  = Or Value Value 
  | And Value Value
  | Rshift Value Value
  | Lshift Value Value
  | Not Value
  | Set Value
  deriving (Show)

type Conn = (String, Op)

main = runSoln' process ((M.! "a") . run) part2

part2 :: Map String Op -> Int
part2 cs = run (M.insert "b" (Set $ Lit $ run cs M.! "a") cs) M.! "a"

-- This relies on sharing for performence.
run :: Map String Op -> Map String Int
run cs = M.map (\v -> (2^16 - 1) .&. resolve v) cs
  where
    value :: Value -> Int
    value (Lit n) = n
    value (Wire x) = run cs M.! x
    resolve :: Op -> Int
    resolve (Or a b) = value a .|. value b
    resolve (And a b) = value a .&. value b
    resolve (Rshift a b) = value a `shiftR` value b
    resolve (Lshift a b) = value a `shiftL` value b
    resolve (Not a) = complement $ value a
    resolve (Set a) = value a

process :: String -> Map String Op
process xs = either (error . errorBundlePretty) id $ runParser pCircuit "input" xs

pCircuit :: Parser (Map String Op)
pCircuit = M.fromList <$> pOp `endBy` newline

symbol :: String -> Parser String
symbol = L.symbol' hspace

pWire :: Parser Value
pWire = L.lexeme hspace1 $ try (Wire <$> some lowerChar) <|> Lit <$> L.decimal

pOp :: Parser (String, Op)
pOp = flip (,) <$> (ops <*> pWire <* symbol "->") <*> L.lexeme hspace (many lowerChar)
  where
    ops :: Parser (Value -> Op)
    ops = choice [
        try $ Lshift <$> pWire <* symbol "lshift",
        try $ Rshift <$> pWire <* symbol "rshift",
        try $ Or <$> pWire <* symbol "or",
        try $ And <$> pWire <* symbol "and",
        Not <$ symbol "not",
        pure Set
      ]

module LambdaParse where

import Prelude hiding (abs)
import qualified Lambda
import Text.Parsec as P
import Text.Parsec.String

exprParser :: Parser Lambda.Expr
exprParser = spaces *> expr <* spaces <* eof

expr :: Parser Lambda.Expr
expr = do
  t <- term
  ts <- many $ many1 space *> term
  return $ foldr (flip Lambda.App) t (reverse ts)

term :: Parser Lambda.Expr
term = try var <|> try abs <|> paren expr

paren :: Parser a -> Parser a
paren = between (char '(') (char ')')

vname :: Parser Lambda.Vname
vname = many1 letter

var :: Parser Lambda.Expr
var = Lambda.Var <$> vname

abs :: Parser Lambda.Expr
abs = do
  char '\\'
  x <- vname
  spaces
  string "->"
  spaces
  e <- expr
  return $ Lambda.Abs x e

parse :: String -> Either ParseError Lambda.Expr
parse = P.parse exprParser ""

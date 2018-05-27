module Data.Lambda.Simple.Parser where

import Prelude hiding (abs)
import qualified Data.Lambda.Simple as Lambda
import Text.Parsec as P hiding (token)

type Parser = Parsec String ()

exprParser :: Parser Lambda.Expr
exprParser = expr <* eof

expr :: Parser Lambda.Expr
expr = do
  t <- term
  ts <- many term
  return $ foldr (flip Lambda.App) t (reverse ts)

term :: Parser Lambda.Expr
term = var <|> abs <|> paren expr

token :: Parser a -> Parser a
token t = t <* spaces

symbol :: String -> Parser String
symbol = token . string

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

vname :: Parser Lambda.Vname
vname = token $ many1 letter

var :: Parser Lambda.Expr
var = Lambda.Var <$> vname

abs :: Parser Lambda.Expr
abs = do
  symbol "\\"
  x <- vname
  symbol "->"
  e <- expr
  return $ Lambda.Abs x e

parse :: String -> Either ParseError Lambda.Expr
parse = P.parse exprParser ""

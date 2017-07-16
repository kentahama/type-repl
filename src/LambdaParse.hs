module LambdaParse where

import Prelude hiding (abs)
import qualified Lambda
import Text.Parsec as P

exprParser :: Parsec String u Lambda.Expr
exprParser = do
  e <- expr
  eof
  return e

expr :: Parsec String u Lambda.Expr
expr = var <|> abs <|> app

vname :: Parsec String u Lambda.Vname
vname = many1 letter

var :: Parsec String u Lambda.Expr
var = do
  x <- try vname
  return $ Lambda.Var x

abs :: Parsec String u Lambda.Expr
abs = do
  try $ char '\\'
  x <- vname
  spaces
  string "->"
  spaces
  e <- expr
  return $ Lambda.Abs x e

app :: Parsec String u Lambda.Expr
app = do
  try $ char '('
  e1 <- expr
  many1 space
  e2 <- expr
  char ')'
  return $ Lambda.App e1 e2

parse :: String -> Either ParseError Lambda.Expr
parse = P.parse exprParser ""

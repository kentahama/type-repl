module Data.Lambda.Simple.TypingTest where

import Data.Lambda.Simple
import Data.Lambda.Simple.Typing

termK :: Expr
termK = Abs "x" $ Abs "y" $ Var "x"

termS :: Expr
termS = Abs "x" $ Abs "y" $ Abs "z" $
  App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

f0 :: Expr
f0 = Abs "x" $ Abs "y" $ App (App (Var "y") (Var "x")) (Var "x")

-- f . g = \x -> f (g x)
-- (...) f g x = f (g x)
(...) :: Expr -> Expr -> Expr
f ... g = Abs "x" $ App f $ App g $ Var "x"

f1 :: Expr
f1 = f0 ... f0

f2 :: Expr
f2 = f1 ... f1

f3 :: Expr
f3 = f2 ... f2

f4 :: Expr
f4 = f3 ... f3

f5 :: Expr
f5 = f4 ... f4

putExprAndType :: String -> Expr -> IO ()
putExprAndType name term = do
  putStr $ name ++ " = "
  print term
  putStr $ name ++ " :: "
  print $ typeof term

main :: IO ()
main = do
  putExprAndType "K" termK
  putExprAndType "S" termS
  putExprAndType "f0" f0
  putExprAndType "f1" f1
  putExprAndType "f2" f2
  putExprAndType "f3" f3
--  putExprAndType "f4" f4
--  putExprAndType "f5" f5

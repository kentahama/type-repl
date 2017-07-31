module Data.Lambda.SimpleTest where

import Test.Hspec
import Data.Lambda.Simple

type1 :: Type
type1 = Arrow (Arrow (Arrow (TypeVar "A") (TypeVar "B")) (TypeVar "C")) (TypeVar "D")

type2 :: Type
type2 = Arrow (TypeVar "A") (Arrow (TypeVar "B") (Arrow (TypeVar "C") (TypeVar "D")))

expr1 :: Expr
expr1 = App (App (App (Var "x") (Var "y")) (Var "z")) (Var "w")

expr2 :: Expr
expr2 = App (Var "x") (App (Var "y") (App (Var "z") (Var "w")))

mainTests :: IO ()
mainTests = hspec $ do
  describe "showType" $ do
    it "returns the string that denotes a given type" $ do
      show (TypeVar "A") `shouldBe` "A"
      show type1 `shouldBe` "((A -> B) -> C) -> D"
      show type2 `shouldBe` "A -> B -> C -> D"
  describe "showExpr" $ do
    it "returns the string that denotes a given term" $ do
      show (Var "x") `shouldBe` "x"
      show expr1 `shouldBe` "x y z w"
      show expr2 `shouldBe` "x (y (z w))"

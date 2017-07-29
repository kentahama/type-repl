module Data.Lambda.Simple.TypingTest where

import Test.Hspec
import Data.Lambda.Simple
import Data.Lambda.Simple.Typing

termK :: Expr
termK = Abs "x" $ Abs "y" $ Var "x"

typeofTermK :: Type
typeofTermK = Arrow (TypeVar "A") (Arrow (TypeVar "B") (TypeVar "A"))

termS :: Expr
termS = Abs "x" $ Abs "y" $ Abs "z" $
  App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

typeofTermS :: Type
typeofTermS = Arrow (Arrow (TypeVar "A") (Arrow (TypeVar "B") (TypeVar "C")))
  (Arrow (Arrow (TypeVar "A") (TypeVar "B")) (Arrow (TypeVar "A") (TypeVar "C")))

mainTests :: IO ()
mainTests = hspec $ do
  describe "typeof" $ do
    it "returns the type of a given term" $ do
      typeof termK `shouldBe` typeofTermK
      typeof termS `shouldBe` typeofTermS

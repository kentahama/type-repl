module Data.Lambda.Simple.TypingTest where

import Test.Hspec
import Data.Either
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
      typeof termK `shouldBe` Right typeofTermK
      typeof termS `shouldBe` Right typeofTermS
    it "returns an error when given non-typable term" $ do
      typeof (Var "x") `shouldSatisfy` isLeft
      typeof (Abs "x" (App (Var "x") (Var "x"))) `shouldSatisfy` isLeft

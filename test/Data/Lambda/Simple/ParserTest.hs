module Data.Lambda.Simple.ParserTest where

import Test.Hspec
import Data.Either
import Data.Lambda.Simple
import Data.Lambda.Simple.Parser

termS :: Expr
termS = Abs "x" $ Abs "y" $ Abs "z" $
  App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

mainTests = hspec $ do
  describe "parse" $ do
    it "returns the parsed lambda expression when given string" $ do
      parse "\\x -> x" `shouldBe`
        (Right $ Abs "x" (Var "x"))
      parse "\\f -> \\x -> (f x)" `shouldBe`
        (Right $ Abs "f" $ Abs "x" $ App (Var "f") (Var "x"))
      parse "(\\x -> x x)" `shouldBe`
        (Right $ Abs "x" $ App (Var "x") (Var "x"))
      parse "\\x -> \\y -> \\z -> ((x z) (y z))" `shouldBe` Right termS
      parse "\\x -> \\y -> \\z -> x z (y z)" `shouldBe` Right termS
    it "returns some parse-error when given invalid expression" $ do
      parse "a -> b" `shouldSatisfy` isLeft
      parse "\\x x" `shouldSatisfy` isLeft

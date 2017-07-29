module Control.SyntacticUnificationTest where

import Test.Hspec
import Control.SyntacticUnification

prob1 :: Problem
prob1 = [
  (T "f" [T "a" [], V "y"],
   T "f" [V "x", T "g" [T "b" []]])
  ]

mgu1 :: Subst
mgu1 = [
  ("y", T "g" [T "b" []]),
  ("x", T "a" [])
  ]

term1 :: Term
term1 = T "f" [V "x", V "y"]

sTerm1 :: Term
sTerm1 = T "f" [T "a" [], T "g" [T "b" []]]

mainTests :: IO ()
mainTests = hspec $ do
  describe "unify" $ do
    it "returns the mgu when given a unification problem" $
      unify prob1 `shouldBe` mgu1
  describe "lift" $ do
    it "returns a substituted term when given a substitution and a term" $
      lift mgu1 term1 `shouldBe` sTerm1

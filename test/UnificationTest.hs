module UnificationTest where

import Unification
import Data.List

prob1 :: Problem
prob1 = [
  (T "f" [T "a" [], V "y"],
   T "f" [V "x", T "g" [T "b" []]])
  ]

mgu1 :: Subst
mgu1 = unify prob1

term1 :: Term
term1 = T "f" [V "x", V "y"]

substedTerm1 :: Term
substedTerm1 = lift mgu1 term1

main :: IO ()
main = do
  putStrLn "Problem 1: "
  putStrLn $ showProblem prob1
  putStrLn "mgu 1: "
  putStrLn $ showSubst mgu1
  putStrLn "Term 1: "
  putStrLn $ showTerm term1
  putStrLn "Substituted term 1: "
  putStrLn $ showTerm substedTerm1

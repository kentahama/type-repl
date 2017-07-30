module Control.SyntacticUnification where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Control.Arrow (second)
import Control.Monad.State (State, modify, runState)

type Vname = String
type Funcname = String

data Term = V Vname | T Funcname [Term] deriving (Eq)
type Subst = [(Vname, Term)]
type Problem = [(Term, Term)]

data UnifyResult = OK
                 | OccurError String
                 | FuncNameDifferError String

showTerm :: Term -> String
showTerm (V x) = x
showTerm (T f ts) = f ++ "(" ++ showArgs ts ++ ")" where
  showArgs = intercalate ", " . map showTerm

instance Show Term where
  show = showTerm

showSubst :: Subst -> String
showSubst s = "{" ++ showMaps s ++ "}" where
  showMaps = intercalate ", " . map showMap
  showMap (x,y) = x ++ " |-> " ++ showTerm y

showProblem :: Problem -> String
showProblem p = "{" ++ showEqus p ++ "}" where
  showEqus = intercalate ", " . map showEqu
  showEqu (s, t) = showTerm s ++ " =? " ++ showTerm t

indom :: Vname -> Subst -> Bool
indom x = any ((==x) . fst)

app :: Subst -> Vname -> Term
app s x = fromMaybe (V x) $ lookup x s

lift :: Subst -> Term -> Term
lift s (V x) = app s x
lift s (T f ts) = T f (map (lift s) ts)

mapProb :: (Term -> Term) -> Problem -> Problem
mapProb f = map (mapTuple f) where
  mapTuple f (t1, t2) = (f t1, f t2)

occurs :: Vname -> Term -> Bool
occurs x (V y) = x == y
occurs x (T _ ts) = any (occurs x) ts

solve :: Problem -> State Subst UnifyResult
solve [] = return OK
solve ((V x, t):ps) = if V x == t
  then solve ps
  else elim x t ps
solve ((t, V x):ps) = elim x t ps
solve ((T f ts, T g us):ps) | f == g = solve (zip ts us ++ ps)
solve ((T f ts, T g us):ps) = return $ FuncNameDifferError $ f ++ " differs from " ++ g

elim :: Vname -> Term -> Problem -> State Subst UnifyResult
elim x t ps | occurs x t = return $ OccurError $ x ++ " is in " ++ show t
elim x t ps = do
  let x2t = lift [(x, t)]
  modify $ map (second x2t)
  modify $ (:) (x, t)
  solve (mapProb x2t ps)

unify :: Problem -> Either String Subst
unify ps = esubst where
  (result, subst) = runState (solve ps) []
  esubst = case result of
    OK -> Right subst
    FuncNameDifferError err -> Left err
    OccurError err -> Left err

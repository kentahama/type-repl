{-# LANGUAGE FlexibleContexts #-}
module Data.Lambda.Simple.Typing where

import Data.Lambda.Simple
import Control.SyntacticUnification hiding (Vname)

import Control.Monad.RWS hiding (lift)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Control.Arrow ((***))

type Context = [(Vname, Type)]
type Constraint = [(Type, Type)]

newVar :: MonadState Int m => m Type
newVar = do
  i <- get
  put $ i + 1
  return . TypeVar $ 't' : show i

addConstraint :: MonadWriter Constraint m =>
                 Type -> Type -> m ()
addConstraint a b = tell [(a, b)]

addCtx :: Vname -> Type -> Context -> Context
addCtx x a = (:) (x, a)

typing :: Expr -> RWS Context Constraint Int Type
typing t = case t of
  Var x -> do
    maybet <- reader $ lookup x
    case maybet of
      Just t -> return t
      Nothing -> fail $ x ++ " is not in the context."
  Abs x e -> do
    alpha <- newVar
    t <- local (addCtx x alpha) (typing e)
    return $ alpha `Arrow` t
  App e1 e2 -> do
    t1 <- typing e1
    t2 <- typing e2
    beta <- newVar
    addConstraint t1 (t2 `Arrow` beta)
    return beta

typeof :: Expr -> Type
typeof e = renameVars $ resolve typ cnstr where
  (typ, _, cnstr) = runRWS (typing e) initialCtx initialVarNum
  initialCtx = []
  initialVarNum = 0

resolve :: Type -> Constraint -> Type
resolve t c = toType $ lift subst $ fromType t where
  subst = unify $ fromConstr c

fromType :: Type -> Term
fromType (TypeVar a) = V a
fromType (Arrow t1 t2) = T "->" [fromType t1, fromType t2]

toType :: Term -> Type
toType (V a) = TypeVar a
toType (T "->" [t1, t2]) = Arrow (toType t1) (toType t2)

fromConstr :: Constraint -> Problem
fromConstr = map (fromType *** fromType)

renameVars :: Type -> Type
renameVars t = flip app t $ zip (nub (occurVars t)) vars where
  app :: [(TypeVname, TypeVname)] -> Type -> Type
  app fs (TypeVar a) = TypeVar . fromMaybe a $ lookup a fs
  app fs (Arrow t1 t2) = Arrow (app fs t1) (app fs t2)
  occurVars :: Type -> [TypeVname]
  occurVars (TypeVar a) = [a]
  occurVars (Arrow t1 t2) = occurVars t1 ++ occurVars t2
  vars :: [TypeVname]
  vars = [[v] | v <- ['A'..'Z']]

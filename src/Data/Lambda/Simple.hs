module Data.Lambda.Simple where

type TypeVname = String
type Vname = String

data Type = TypeVar TypeVname
          | Arrow Type Type deriving Eq

data Expr = Var Vname
          | Abs Vname Expr
          | App Expr Expr deriving Eq

showType :: Type -> String
showType (TypeVar a) = a
showType (Arrow (Arrow t1 t2) t3) = concat
  ["(", showType (Arrow t1 t2), ") -> ", showType t3]
showType (Arrow t1 t2) = concat
  [showType t1, " -> ", showType t2]

instance Show Type where
  show = showType

showExpr :: Expr -> String
showExpr (Var x) = x
showExpr (Abs x e) = concat
  ["(\\", x, " -> ", showExpr e, ")"]
showExpr (App e1 (App e2 e3)) = concat
  [showExpr e1, " (", showExpr (App e2 e3), ")"]
showExpr (App e1 e2) = concat
  [showExpr e1, " ", showExpr e2]

instance Show Expr where
  show = showExpr

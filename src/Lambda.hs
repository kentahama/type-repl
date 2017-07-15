module Lambda where

type TypeVname = String
type Vname = String

data Type = TypeVar TypeVname
          | Arrow Type Type deriving Eq

data Expr = Var Vname
          | Abs Vname Expr
          | App Expr Expr

showType :: Type -> String
showType (TypeVar a) = a
showType (Arrow t1 t2) = concat
  ["(", show t1, "->", show t2, ")"]

instance Show Type where
  show = showType

showExpr :: Expr -> String
showExpr (Var x) = x
showExpr (Abs x e) = concat
  ["(\\", x, " -> ", showExpr e, ")"]
showExpr (App e1 e2) = concat
  ["(", showExpr e1, " ", showExpr e2, ")"]

instance Show Expr where
  show = showExpr

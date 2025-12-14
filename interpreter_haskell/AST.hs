module AST where

data Op 
    = Add | Sub | Mul | Div | Mod
    | And | Or 
    | Lt | Gt | Le | Ge | Eq | NeQ
    deriving (Show, Eq)

data UnaryOp
    = Not | Neg
    deriving (Show, Eq)

data Expr 
    = Number Double
    | Boolean Bool
    | Binary Op Expr Expr
    | Unary UnaryOp Expr
    | Identifier String
    | Assign String Expr
    deriving (Show, Eq)

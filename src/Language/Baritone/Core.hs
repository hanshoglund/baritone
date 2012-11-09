
module Language.Baritone.Core where


data Expr
    = Var String
    | App [Expr]
    | Abs String Expr
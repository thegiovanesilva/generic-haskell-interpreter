module Lexer where

data Ty = TBool
        | TNum
        deriving (Show, Eq)

-- Abstract Syntax Tree

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr Expr
          deriving Show 
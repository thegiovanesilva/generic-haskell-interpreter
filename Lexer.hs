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

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenAdd
           | TokenAnd
           | TokenIf
           | TokenThen
           | TokenElse
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlpha c = lexKW (c:cs)

lexer ('+':cs) = TokenAdd : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer _ = error "Lexical error"

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
            | ("true", rest) -> TokenTrue
            | ("false", rest) -> TokenFalse
            | ("if", rest) -> TokenIf
            | ("then", rest) -> TokenThen
            | ("else", rest) -> TokenElse
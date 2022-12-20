
module Lexer where 

import Data.Char 

data Ty = TBool
        | TNum
        deriving (Show, Eq)

data Expr = BTrue
          | BFalse
          | Num Int 
          | Add Expr Expr 
          | And Expr Expr 
          | If Expr Expr Expr
          | Var String
          | Lam String Expr
          | App Expr Expr
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
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKW (c:cs)
lexer ('+':cs) = TokenAdd : lexer cs 
lexer ('&':cs) = TokenAnd : lexer cs
lexer _ = error "Lexical error: caracter inválido!"

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest)  -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest 
             ("if", rest)    -> TokenIf : lexer rest 
             ("then", rest)  -> TokenThen : lexer rest 
             ("else", rest)  -> TokenElse : lexer rest 
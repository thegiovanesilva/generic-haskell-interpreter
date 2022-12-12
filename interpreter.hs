module Interpreter where

-- Abstract Syntax Tree

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | If Exp Expr Expr
          deriving Show 

step :: Expr -> Maybe Expr
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n1) e2) = case step e2 of
                            Just e2' -> Just (Add (Num n1) e2')
                            _        -> Nothing 

step (Add e1 e2) = case step e1 of
                    Just e1' -> Just (Add e1' e2)
                    _        -> Nothing          

step (And BTrue e2) = Just e2
step (And BFalse _) = Just BFalse

step (And e1 e2) = case step e1 of
                    Just e1' -> Just (And e1' e2)
                    _        -> Nothing

step e = Just e


step (Or BTrue _) = Just BTrue
step (Or BFalse e2) = Just e2
step (Or e1 e2) = case step e1 of
                   Just e1 -> (Or e1' e2)
                   _       -> Nothing

step e = Just e

step (If BTrue e1 _)
step (If BFalse _ e2)
step (If e e1 e2) =  case step e of
                      Just e' -> Just (If e' e1 e2)
                      _       -> Nothing

step e = Just e
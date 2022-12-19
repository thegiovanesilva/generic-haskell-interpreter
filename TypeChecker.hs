module TypeChecker where 

import Lexer

typeof :: Expr -> Maybe Ty
typeof BTrue = Just TBool 
typeof BFalse = Just TBool 
typeof (Num _) = Just TNum
typeof (Add e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TNum, Just TNum) -> Just TNum
                       _                       -> Nothing 
typeof (And e1 e2) = case (typeof e1, typeof e2) of 
                       (Just TBool, Just TBool) -> Just TBool 
                       _                         -> Nothing
typeof (If e e1 e2) = 
    case typeof e of 
      Just TBool -> case (typeof e1, typeof e2) of 
                      (Just t1, Just t2) -> if t1 == t2 then
                                              Just t1 
                                            else 
                                              Nothing
                      _                  -> Nothing 
      _          -> Nothing

typecheck :: Expr -> Expr
typecheck e = case typeof e of
              Just _ -> e
              _      -> error "Type error"


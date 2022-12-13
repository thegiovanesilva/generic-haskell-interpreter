{

module Parser where

import Lexer

}

%name parser
%tokentype { Token }
%error { parseError }

%token 
    num             { TokenNum $$ }
    '+'             { TokenAdd }
    '&'             { TokenAnd }
    "true"          { TokenTrue}
    "false"         { TokenFalse}
    "if"            { TokenIf }
    "then"          { TokenThen }
    "else"          { TokenElse }

%%

Exp     : num                       { Num $1 }
        | false                     { BFalse }
        | true                      { BTrue }
        
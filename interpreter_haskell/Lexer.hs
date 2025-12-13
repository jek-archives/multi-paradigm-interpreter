module Lexer where

import Data.Char

data Token 
    = TokNum Double
    | TokPlus
    | TokMinus
    | TokMul
    | TokDiv
    | TokMod
    | TokLParen
    | TokRParen
    | TokAssign
    | TokEq
    | TokNEq
    | TokLT
    | TokGT
    | TokLE
    | TokGE
    | TokAnd
    | TokOr
    | TokNot
    | TokTrue
    | TokFalse
    | TokIdent String
    | TokEnd
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = [TokEnd]
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = 
        let (numStr, rest) = span (\x -> isDigit x || x == '.') (c:cs)
        in TokNum (read numStr) : lexer rest
    | isAlpha c = 
        let (str, rest) = span isAlphaNum (c:cs)
        in case str of
            "and" -> TokAnd : lexer rest
            "or" -> TokOr : lexer rest
            "not" -> TokNot : lexer rest
            "true" -> TokTrue : lexer rest
            "false" -> TokFalse : lexer rest
            _ -> TokIdent str : lexer rest
    | c == '+' = TokPlus : lexer cs
    | c == '-' = TokMinus : lexer cs
    | c == '*' = TokMul : lexer cs
    | c == '/' = TokDiv : lexer cs
    | c == '%' = TokMod : lexer cs
    | c == '(' = TokLParen : lexer cs
    | c == ')' = TokRParen : lexer cs
    | c == '=' = 
        case cs of
            ('=':rest) -> TokEq : lexer rest
            _ -> TokAssign : lexer cs
    | c == '!' = 
        case cs of
            ('=':rest) -> TokNEq : lexer rest
            _ -> error "Unexpected character '!'"
    | c == '<' = 
        case cs of
            ('=':rest) -> TokLE : lexer rest
            _ -> TokLT : lexer cs
    | c == '>' = 
        case cs of
            ('=':rest) -> TokGE : lexer rest
            _ -> TokGT : lexer cs
    | otherwise = error $ "Unknown character: " ++ [c]

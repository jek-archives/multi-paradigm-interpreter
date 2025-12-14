module Parser where

import AST
import Lexer

parse :: [Token] -> Expr
parse tokens = fst (parseExpression tokens)

-- Expression (Assignment or Logical OR)
parseExpression :: [Token] -> (Expr, [Token])
parseExpression (TokIdent name : TokAssign : rest) = 
    let (expr, rest') = parseExpression rest
    in (Assign name expr, rest')
parseExpression tokens = parseLogicalOr tokens

-- Logical OR
parseLogicalOr :: [Token] -> (Expr, [Token])
parseLogicalOr tokens = 
    let (left, rest) = parseLogicalAnd tokens
    in case rest of
        (TokOr:rest') -> 
            let (right, rest'') = parseLogicalOr rest'
            in (Binary Or left right, rest'')
        _ -> (left, rest)

-- Logical AND
parseLogicalAnd :: [Token] -> (Expr, [Token])
parseLogicalAnd tokens = 
    let (left, rest) = parseEquality tokens
    in case rest of
        (TokAnd:rest') -> 
            let (right, rest'') = parseLogicalAnd rest'
            in (Binary And left right, rest'')
        _ -> (left, rest)

-- Equality
parseEquality :: [Token] -> (Expr, [Token])
parseEquality tokens = 
    let (left, rest) = parseRelational tokens
    in case rest of
        (TokEq:rest') -> 
            let (right, rest'') = parseEquality rest'
            in (Binary Eq left right, rest'')
        (TokNEq:rest') -> 
            let (right, rest'') = parseEquality rest'
            in (Binary NeQ left right, rest'')
        _ -> (left, rest)

-- Relational
parseRelational :: [Token] -> (Expr, [Token])
parseRelational tokens = 
    let (left, rest) = parseAdditive tokens
    in case rest of
        (TokLT:rest') -> 
            let (right, rest'') = parseRelational rest'
            in (Binary Lt left right, rest'')
        (TokGT:rest') -> 
            let (right, rest'') = parseRelational rest'
            in (Binary Gt left right, rest'')
        (TokLE:rest') -> 
            let (right, rest'') = parseRelational rest'
            in (Binary Le left right, rest'')
        (TokGE:rest') -> 
            let (right, rest'') = parseRelational rest'
            in (Binary Ge left right, rest'')
        _ -> (left, rest)

-- Additive
parseAdditive :: [Token] -> (Expr, [Token])
parseAdditive tokens = 
    let (left, rest) = parseMultiplicative tokens
    in case rest of
        (TokPlus:rest') -> 
            let (right, rest'') = parseAdditive rest'
            in (Binary Add left right, rest'')
        (TokMinus:rest') -> 
            let (right, rest'') = parseAdditive rest'
            in (Binary Sub left right, rest'')
        _ -> (left, rest)

-- Multiplicative
parseMultiplicative :: [Token] -> (Expr, [Token])
parseMultiplicative tokens = 
    let (left, rest) = parseUnary tokens
    in case rest of
        (TokMul:rest') -> 
            let (right, rest'') = parseMultiplicative rest'
            in (Binary Mul left right, rest'')
        (TokDiv:rest') -> 
            let (right, rest'') = parseMultiplicative rest'
            in (Binary Div left right, rest'')
        (TokMod:rest') -> 
            let (right, rest'') = parseMultiplicative rest'
            in (Binary Mod left right, rest'')
        _ -> (left, rest)

-- Unary
parseUnary :: [Token] -> (Expr, [Token])
parseUnary (TokMinus:rest) = 
    let (expr, rest') = parseUnary rest
    in (Unary Neg expr, rest')
parseUnary (TokNot:rest) = 
    let (expr, rest') = parseUnary rest
    in (Unary Not expr, rest')
parseUnary tokens = parsePrimary tokens

-- Primary
parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (TokNum n : rest) = (Number n, rest)
parsePrimary (TokTrue : rest) = (Boolean True, rest)
parsePrimary (TokFalse : rest) = (Boolean False, rest)
parsePrimary (TokIdent name : rest) = (Identifier name, rest)
parsePrimary (TokLParen : rest) = 
    let (expr, (TokRParen:rest')) = parseExpression rest
    in (expr, rest')
parsePrimary _ = error "Syntax error: Unexpected token"

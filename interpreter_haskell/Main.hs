module Main where

import Lexer
import Parser
import Eval
import System.IO

main :: IO ()
main = do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    if line == "exit" 
        then return ()
        else do
            let tokens = lexer line
            let ast = parse tokens
            let result = eval ast
            print result
            main

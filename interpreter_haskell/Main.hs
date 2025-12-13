module Main where

import Lexer
import Parser
import Eval
import System.IO

main :: IO ()
main = do
    putStrLn "Haskell Interpreter REPL."
    putStrLn "Commands: 'debug' to toggle AST view, 'exit' to quit."
    repl False

repl :: Bool -> IO ()
repl debug = do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    case line of
        "exit" -> return ()
        "debug" -> do
            let newDebug = not debug
            putStrLn $ "Debug Mode: " ++ if newDebug then "ON" else "OFF"
            repl newDebug
        _ -> do
            let tokens = lexer line
            let ast = parse tokens
            if debug 
                then do
                    putStrLn $ "Tokens: " ++ show tokens
                    putStrLn $ "AST: " ++ show ast
                else return ()
            
            let result = eval ast
            print result
            repl debug

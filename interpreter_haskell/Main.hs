module Main where

import Lexer
import Parser
import Eval
import System.IO
import qualified Data.Map as Map
import System.IO.Error
import Control.Exception

main :: IO ()
main = do
    putStrLn "Haskell Interpreter REPL."
    putStrLn "Commands: 'debug' to toggle AST view, 'exit' to quit."
    repl False Map.empty

repl debug env = do
    putStr ">>> "
    hFlush stdout
    result <- try (getLine) :: IO (Either IOError String)
    case result of
        Left e -> 
            if isEOFError e
                then return ()
                else ioError e
        Right line -> do
            case line of
                "exit" -> putStrLn "Exited."
                "debug" -> do
                    let newDebug = not debug
                    putStrLn $ "Debug Mode: " ++ if newDebug then "ON" else "OFF"
                    repl newDebug env
                _ -> do
                    let tokens = lexer line
                    let ast = parse tokens
                    if debug 
                        then do
                            putStrLn $ "Tokens: " ++ show tokens
                            putStrLn $ "AST: " ++ show ast
                        else return ()
                    
                    let (result, newEnv) = eval ast env
                    print result
                    repl debug newEnv

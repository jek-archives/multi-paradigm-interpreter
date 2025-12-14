import sys
import io
from lexer import Lexer
from parser import Parser
from interpreter import Interpreter
from errors import LexerError, ParserError, InterpreterError

def test_error(text, expected_error_type, description):
    print(f"Testing {description} with input: '{text}'")
    try:
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        interpreter = Interpreter()
        interpreter.visit(tree)
        print("FAIL: No error raised")
    except Exception as e:
        if isinstance(e, expected_error_type):
            print(f"PASS: Caught expected {type(e).__name__}: {e}")
            if hasattr(e, 'lineno') and e.lineno is not None:
                print(f"      Location: Line {e.lineno}, Col {e.column}")
        else:
            print(f"FAIL: Caught unexpected error {type(e).__name__}: {e}")
    print("-" * 20)

if __name__ == "__main__":
    # 1. Lexer Error
    test_error("@", LexerError, "Lexer Error (invalid char)")
    
    # 2. Parser Error
    test_error("1 + * 2", ParserError, "Parser Error (syntax)")
    
    # 3. Interpreter Error
    test_error("x", InterpreterError, "Interpreter Error (undefined var)")
    
    # 4. Valid case
    print("Testing Valid Case: 1 + 2")
    try:
        lexer = Lexer("1 + 2")
        parser = Parser(lexer)
        tree = parser.parse()
        interpreter = Interpreter()
        res = interpreter.visit(tree)
        print(f"PASS: Result is {res}")
    except Exception as e:
        print(f"FAIL: Unexpected error {e}")

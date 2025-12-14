from lexer import Lexer
from parser import Parser
from interpreter import Interpreter

def run_test(name, inputs):
    print(f"--- {name} ---")
    interpreter = Interpreter()
    for code in inputs:
        print(f">>> {code}")
        try:
            lexer = Lexer(code)
            parser = Parser(lexer)
            tree = parser.parse()
            if tree:
                result = interpreter.visit(tree)
                print(f"Result: {result}")
        except Exception as e:
            print(f"Error: {e}")
    print()

if __name__ == '__main__':
    # 1. Advanced Arithmetic (Precedence)
    run_test("Complex Arithmetic", [
        "10 + 5 * 2",         # Should be 20, not 30
        "(10 + 5) * 2",       # Should be 30
        "100 / 2 / 2",        # Should be 25 (Left associative)
        "4 + 3 * (10 - 2)"    # Should be 28
    ])

    # 2. Variables
    run_test("Variable Usage", [
        "x = 10",             # Assignment
        "y = 20",             # Assignment
        "x + y",              # Usage (30)
        "z = x * y + 5",      # Complex assignment (205)
        "z"                   # Check z
    ])

    # 3. Mixed Logic
    run_test("Mixed Logic", [
        "(5 > 3) and (10 < 20)", # True and True -> True
        "not (5 == 5)",          # False
        "x = true",
        "y = false",
        "x or y"                 # True
    ])

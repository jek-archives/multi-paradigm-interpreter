from lexer import Lexer
from parser import Parser
from interpreter import Interpreter
import readline

def main():
    interpreter = Interpreter()
    print("Python Interpreter REPL.")
    print("Commands: 'debug' to toggle AST view, 'exit' to quit.")
    
    debug_mode = False
    
    while True:
        try:
            text = input('>>> ')
        except EOFError:
            break
            
        if not text:
            continue
            
        if text == 'exit':
            break
            
        if text == 'debug':
            debug_mode = not debug_mode
            print(f"Debug Mode: {'ON' if debug_mode else 'OFF'}")
            continue

        try:
            lexer = Lexer(text)
            
            if debug_mode:
                # Store tokens for display then re-initialize for parser
                tokens = []
                while True:
                    tok = lexer.get_next_token()
                    tokens.append(tok)
                    if tok.type.name == 'EOF': break
                print(f"Tokens: {tokens}")
                # Reset lexer for parsing
                lexer = Lexer(text)

            parser = Parser(lexer)
            tree = parser.parse() # changed from expr() to parse()
            
            if debug_mode:
                print(f"AST: {tree}")
            
            result = interpreter.visit(tree)
            print(f"Result: {result}")
        except Exception as e:
            print(f"Error: {e}")

if __name__ == '__main__':
    main()

from lexer import Lexer
from parser import Parser
from interpreter import Interpreter
from errors import Error
try:
    import readline
except ImportError:
    pass

def main():
    interpreter = Interpreter()
    print("Python Interpreter REPL.")
    print("Commands: 'debug' to toggle AST view, 'exit' to quit.")
    
    debug_mode = False
    
    while True:
        try:
            text = input('>>> ')
        except (EOFError, KeyboardInterrupt):
            # break to exit loop and fall through to goodbye
            break
            
        if not text:
            continue
            
        if text == 'exit':
            print("Exited.")
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
        except Error as e:
            print(f"{type(e).__name__}: {e.message}")
            if e.lineno is not None and e.column is not None:
                # Ideally we would show the line, but in REPL we only have 'text' for the current line(s)
                # If we had multiline input we'd split text by lines.
                # Here we assume single line REPL for simplicity or loop over lines.
                lines = text.split('\n')
                if 0 < e.lineno <= len(lines):
                    line = lines[e.lineno - 1]
                    print(f"  {line}")
                    # column is 1-based usually? Lexer init col=1.
                    # e.column is 0-based index or 1-based? 
                    # update: In Lexer I did column=1 and incremented. 
                    # If I increment AFTER update, then it's 1-based check.
                    # let's assume 1-based.
                    caret_col = max(1, e.column)
                    print(f"  {' ' * (caret_col - 1)}^")
        except Exception as e:
            print(f"Unexpected Error: {e}")


if __name__ == '__main__':
    main()

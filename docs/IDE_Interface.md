# Minimal IDE/REPL Interface Plan

## Design
The interface is a text-based REPL (Read-Eval-Print Loop) running in the terminal.
Each language's `main` file implements this loop.

## Features
1. **Prompt**: Shows `>>> ` to indicate readiness.
2. **Input Reading**: Waits for user to type a line and press Enter.
3. **Command Handling**:
   - `exit`: Terminates the program.
   - `debug`: Toggles verbose mode (shows Token list and AST structure before result).
4. **Output**:
   - Prints result on a new line: `Result: <value>`
   - Prints errors with `Error: <message>`

## Implementation Snippets
### C
```c
if (strcmp(input, "debug") == 0) { debug_mode = !debug_mode; continue; }
```
### Python
```python
if text == 'debug':
    debug_mode = not debug_mode
    continue
```
### Prolog
```prolog
% Pass a Debug flag through the run predicate
```
### Haskell
```haskell
-- Maintain state using a recursive main with arguments
mainLoop debug = do ...
```

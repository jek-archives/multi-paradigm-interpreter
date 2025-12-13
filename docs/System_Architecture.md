# System Architecture

## 1. Components Overview

Each interpreter follows the classic pipeline:
`Source Code (String)` -> **Lexer** -> `Tokens` -> **Parser** -> `AST` -> **Evaluator** -> `Result`

### A. Lexer (Tokenizer)
- **Input**: Raw source string (e.g., `x = 10 + 5`).
- **Output**: Stream/List of Tokens.
- **Token Types**:
    - `NUMBER` (int/float)
    - `IDENTIFIER` (variable names)
    - `KEYWORD` (and, or, not, true, false)
    - `OPERATOR` (+, -, *, /, %, <, >, <=, >=, ==, !=, =)
    - `LPAREN` (
    - `RPAREN` )
    - `EOF`
- **Handling**: Skip whitespace. Error on illegal characters.

### B. Parser
- **Input**: Stream of Tokens.
- **Output**: Abstract Syntax Tree (AST).
- **Algorithm**: Recursive Descent (preferred for simplicity) or Operator Precedence.
- **Structure**:
    - `parseExpression()`
    - `parseTerm()`
    - `parseFactor()`
    - etc., matching the Grammar terms.

### C. AST (Abstract Syntax Tree)
Nodes roughly corresponding to:
- `BinaryOp(left, op, right)`
- `UnaryOp(op, operand)`
- `Literal(value)`
- `Variable(name)`
- `Assignment(name, value_node)`

### D. Evaluator (Interpreter)
- **Input**: AST.
- **Context**: A Symbol Table (Environment) to store variable values.
- **Output**: Value (Number or Boolean).
- **Behavior**:
    - Recursive traversal of the AST.
    - `BinaryOp`: Evaluate left and right, then apply op.
    - `Assignment`: Evaluate result, store in Symbol Table, return value.
    - `Identifier`: Look up in Symbol Table. Error if undefined.

## 2. Error Handling Strategy
- **Lexical Errors**: Catch invalid characters during tokenization. Report line/column.
- **Syntax Errors**: Catch unexpected tokens during parsing. "Expected X, got Y".
- **Runtime Errors**:
    - Division by zero.
    - Undefined variables.
    - Type mismatches (e.g., `true + 5`, unless implicit casting is desired).

## 3. Input/Output (IDE/REPL)
- **Prompt**: `>>> `
- **Commands**:
    - `exit`: Quit the REPL.
    - `debug`: Toggle showing Tokens/AST before result.
    - `history`: Show past commands (Optional).
- **Format**:
    ```text
    >>> 1 + 2 * 3
    Tokens: [NUM(1), PLUS, NUM(2), MULT, NUM(3)] (Iterative/Debug Only)
    AST: BinaryOp(+, 1, BinaryOp(*, 2, 3)) (Iterative/Debug Only)
    Result: 7
    ```

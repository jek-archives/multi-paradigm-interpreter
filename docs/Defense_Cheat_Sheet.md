# 🛡️ Oral Defense Cheat Sheet: "How It Works"

If the professor asks **"Explain your project simply"**, use this guide.

## 1. The Big Picture (Pipeline) 🚂
All 4 languages follow the **exact same 3-step process**. Memorize this flow:

### Step A: Lexer (The "Scanner")
*   **Input**: `10 + 20` (String)
*   **Job**: chop it into chunks (Tokens).
*   **Result**: `[NUMBER(10), PLUS, NUMBER(20)]`
*   *Analogy*: Like reading a sentence and circling the words.

### Step B: Parser (The "Architect")
*   **Input**: The list of tokens.
*   **Job**: Arrange them into a structure/tree based on Grammar Rules.
*   **Result**: An **AST** (Abstract Syntax Tree).
    ```
      (+)
     /   \
   (10)  (20)
    ```
*   *Why a Tree?*: Because trees respect **Order of Operations** (multiplication happens lower in the tree).

### Step C: Evaluator (The "Calculator")
*   **Input**: The Tree.
*   **Job**: Walk down the tree and solve it.
*   **Result**: `30`
*   *Analogy*: Solving the math problem from the bottom up.

---

## 2. Paradigm-Specific Questions 🧠

### "How does the C Version work?" (Imperative)
*   **Keywords**: *Structs, Pointers, Manual Memory*.
*   **Explanation**: "In C, we have to build the Tree manually using `structs` and `malloc`. We create a struct for 'Number' and a struct for 'Operation', then link them with pointers. It's fast, but we have to be careful not to crash."

### "How does the Python Version work?" (Object Oriented)
*   **Keywords**: *Classes, Objects, Inheritance*.
*   **Explanation**: "In Python, every node in the tree is an **Object** (Sequence of Class Inheritance). We have a base class `Node`, and child classes like `BinOp` or `Num`. The `parse()` method creates these objects."

### "How does the Prolog Version work?" (Logic)
*   **Keywords**: *DCG (Definite Clause Grammar), Unification*.
*   **Explanation**: "Prolog is unique. We don't write 'loops'. Instead, we define **Rules**. We say 'An expression IS a term plus another term'. Prolog automatically tries to match the input string against these rules to build the tree. It feels like magic because Prolog does the searching for us."

### "How does the Haskell Version work?" (Functional)
*   **Keywords**: *Pattern Matching, Recursion, Data Types*.
*   **Explanation**: "Haskell is perfect for this. We define a `Type` called `Expr`. Then our evaluator is just a function that pattern matches: 'If you see an Add expression, return left + right'. It's the cleanest code because there is no 'state' or variables changing."

---

## 3. "What was the hardest part?" (Common Q)
*   **Answer**: "Unifying the Grammar."
*   **Why**: "Getting C (which is rigid) and Prolog (which is logic-based) to agree on the exact same syntax rules was difficult. We had to design a single 'Context-Free Grammar' (CFG) first, then implement it 4 times."

## 4. "How does the GUI talk to C?"
*   **Answer**: "Simple Input/Output."
*   **Explanation**: "The Python server runs a command line command (like `./interpreter_c`). It sends the user's code into `stdin` (Standard Input) and captures the `stdout` (Standard Output). The C program doesn't know it's on a web page; it thinks it's running in a terminal."

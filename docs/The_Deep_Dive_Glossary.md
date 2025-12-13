# 🧠 The Deep Dive: "Explain Like I'm 5" (But for CS Students)

This document explains **every single technical term** used in your project. Read this to understand *what* you built.

---

## 🏗️ PART 1: The Building Blocks

### 1. What is an "Interpreter"?
*   **Definition**: A program that reads code and executes it immediately.
*   **vs Compiler**: A Compiler (like GCC) translates code into a binary file (`.exe`) to run *later*. An Interpreter (like Python) runs it *now*.
*   **Your Project**: You built an **Interpreter**. It reads `1+1` and immediately prints `2`.

### 2. What is a "Paradigm"?
*   **Definition**: A "style" or "philosophy" of coding.
*   **Imperative (C)**: "Do this, then do that." (Step-by-step commands).
*   **Object-Oriented (Python)**: "Everything is an object with data and methods." (Modeling the world).
*   **Functional (Haskell)**: "Everything is a math function. No changing variables." (Pure logic).
*   **Logic (Prolog)**: "Define truth rules, let the computer search for answers." (Fact-based).

---

## 🚂 PART 2: The Pipeline (How code becomes action)

Imagine the user types: `10 + 20`

### Step 1: Lexical Analysis ("Lexing")
*   **The Problem**: The computer sees a string `"10 + 20"`. It's just a bunch of characters/bytes. It doesn't know "10" is a number.
*   **The Solution**: The **Lexer**.
*   **How it works**: It loops through the string character by character.
    *   It sees `1`, then `0`. It sees a space afterwards. It groups `1` and `0` and says "This is a NUMBER".
    *   It sees `+`. It says "This is a PLUS".
*   **Output (Tokens)**: A list: `[TOKEN_NUMBER(10), TOKEN_PLUS, TOKEN_NUMBER(20)]`.
*   **Key Term**: **Token** = An atom/unit of meaning (like a "word" in a sentence).

### Step 2: Parsing ("Syntax Analysis")
*   **The Problem**: We have a list of words, but we don't know the *structure*. Does `10 + 20 * 3` mean `(10+20)*3` or `10+(20*3)`?
*   **The Solution**: The **Parser**.
*   **How it works**: It uses **Grammar Rules** (see Part 3) to arrange tokens into a Tree.
*   **Output (AST)**: Authorization Syntax Tree (or Abstract Syntax Tree).
    *   **AST**: A tree structure representing the code.
    *   **Why Abstract?**: It throws away useless stuff like parenthesis `()` or spaces. It only keeps the logic.
    *   Structure for `10 + 20`:
        ```
          BinOp(+)  <-- Root Node
         /       \
       Num(10)   Num(20)  <-- Leaf Nodes
        ```

### Step 3: Evaluation ("Semantics")
*   **The Problem**: We have a tree. Now what?
*   **The Solution**: The **Evaluator** (Recursive Function).
*   **How it works**: It visits the nodes.
    1.  Visits `BinOp(+)`. It asks: "What is my left child?"
    2.  Visits `Num(10)`. Returns `10`.
    3.  Visits `Num(20)`. Returns `20`.
    4.  The `BinOp(+)` adds them: `10 + 20 = 30`.
*   **Output**: The final result `30`.

---

## 📐 PART 3: The Hard Stuff (Grammar)

### 1. Context-Free Grammar (CFG)
*   **Definition**: A set of rules that define valid sentences in a language.
*   **Why "Context-Free"?**: The rules apply regardless of "meaning". `Dog eats car` is grammatically valid (Subject Verb Object), even if it makes no sense.
*   **Your Grammar**:
    *   `Expr -> Term + Expr` (Expressions are things added together)
    *   `Term -> Factor * Term` (Terms are things multiplied)
    *   `Factor -> Number | (Expr)` (Factors are numbers)

### 2. Recursion
*   **Definition**: A function calling itself.
*   **Where you used it**: The **Parser** calls itself.
    *   To parse `(1 + 2)`, `parseFactor` sees a `(`, so it calls `parseExpression` to handle the inside part.
    *   The **Evaluator** calls itself to solve sub-branches of the tree.

### 3. Precedence (Order of Operations)
*   **Definition**: Which operator wins? `*` beats `+`.
*   **How you did it**:
    *   We enforced this by putting `*` (Term) **lower** in the dependency chain than `+` (Expression).
    *   The Parser looks for `+` first, splitting the string. Then it looks for `*` inside the chunks. This forces the chunks to be multiplied *before* being added.

---

## 🧠 PART 4: Language Specifics (Brief)

*   **Struct (C)**: A custom data bucket. `struct Node { int value; }`.
*   **Pointer (C)**: An address in memory (`0x7ffe...`). In C, we don't pass the whole tree around; we pass the *address* of the tree to save speed.
*   **Unification (Prolog)**: Prolog's way of "equation solving". `X = 1 + 1` makes X become 2. It finds values that make the statement true.
*   **Pattern Matching (Haskell)**: Imagine a `switch` statement but for data shapes. "If data looks like (A + B), do this."

---

## 🛡️ Summary for Defense
If you get lost, just say:
> "We take text, lex it into tokens, parse it into a tree structure using recursive rules to handle precedence, and then evaluate the tree to get the number."

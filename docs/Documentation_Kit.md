# Documentation Kit (Copy-Paste Ready)

## 1. Introduction
The Advanced Expression Interpreter is a multi-paradigm software system designed to evaluate mathematical and logical expressions. It is implemented in four distinct programming languages: C (Imperative), Python (Object-Oriented), Prolog (Logic), and Haskell (Functional). This project demonstrates the comparative strengths and weaknesses of each paradigm in solving the same problem: parsing and evaluating a Context-Free Grammar (CFG).

## 2. Objectives
- **Core Objective**: Build a robust interpreter for arithmetic, boolean, and relational expressions.
- **Educational Goal**: Analyze how different paradigms handle recursion, state management, and parsing.
- **Technical Goal**: Ensure consistent behavior across all four implementations using a Unified Grammar.

## 3. System Overview
The system accepts an input string (Source Code), tokenizes it (Lexical Analysis), creates a hierarchical structure (AST Construction), and traverses this structure to compute a result (Evaluation). All versions support a minimal REPL (Read-Eval-Print Loop) interface.

## 4. Grammar Specification
See `Unified_Grammar.md`.

## 5. Components Design
### Lexer
Converts raw text into a stream of tokens `[NUMBER, PLUS, IDENTIFIER, ...]`.
### Parser
Uses Recursive Descent to build an Abstract Syntax Tree (AST) respecting operator precedence.
### Interpreter
Traverses the AST. In C/Python, this is a Visitor or Switch statement. In Haskell/Prolog, this is achieved via Pattern Matching.

## 6. Paradigm Justification
- **C**: Offers low-level control and teaches manual memory management and pointer arithmetic for tree structures.
- **Python**: Demonstrates OOP principles (Classes for Nodes) and rapid development of the REPL.
- **Prolog**: Showcases the power of Definite Clause Grammars (DCG) and unification for parsing logic.
- **Haskell**: Highlights functional purity, immutability, and algebraic data types for defining the AST.

## 7. Testing & Validation
Testing is performed by running the same set of 10 expressions across all 4 interpreters and verifying identical outputs.
Example: `3 + 4 * 2` -> Expect `11` in all systems.

## 8. Conclusion
The project successfully bridges four paradigms. While Python and Haskell offered the most concise syntax for tree manipulation, C provided insights into the underlying mechanics of memory, and Prolog offered a unique, declarative approach to grammar definition.

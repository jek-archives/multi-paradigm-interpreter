# PART I — PROJECT INTRODUCTION & OVERVIEW

## 1. Project Title
**Multi-Paradigm Expression Interpreter: A Comparative Study of Programming Paradigms**

## 2. Project Objective (What the Program Does)
The objective of this project is to design and implement an expression interpreter capable of evaluating:
*   **Arithmetic expressions** (e.g., `+`, `-`, `*`, `/`, `%`)
*   **Relational expressions** (e.g., `<`, `>`, `<=`, `>=`, `==`, `!=`)
*   **Boolean expressions** (e.g., `and`, `or`, `not`)
*   **Variable assignments** (e.g., `x = 10 + 5`)

The interpreter reads user input through a **REPL (Read–Eval–Print Loop)**, processes the input, evaluates the expression, and outputs the result immediately.

All implementations are assumed to run correctly and produce accurate results for valid inputs.

## 3. Core Goal of the Machine Problem (Why This Project Exists)
The primary goal of the machine problem is not only correctness but to **demonstrate how different programming paradigms solve the same computational problem in fundamentally different ways.**

To achieve this, the same interpreter is implemented using four programming paradigms:
1.  **Imperative Paradigm** — C
2.  **Object-Oriented Paradigm** — Python
3.  **Logic Paradigm** — Prolog
4.  **Functional Paradigm** — Haskell

Each implementation follows the same language definition and grammar, ensuring that differences in design come from the paradigm itself, not from changes in requirements.

## 4. Unified Language and Behavior
All four interpreters share the following characteristics:
*   The same grammar and expression rules
*   The same input format
*   The same output behavior
*   The same error categories (lexical, syntax, runtime)

This guarantees a fair comparison between paradigms.

**Example input:**
```text
10 + 5 * 2
```

**Example output:**
```text
20
```

## 5. Why an Interpreter Was Chosen
An expression interpreter was chosen because it naturally involves:
*   Parsing and grammar rules
*   Tree-based data structures (AST)
*   Recursive evaluation
*   Clear separation of concerns

These characteristics make it an ideal problem for comparing paradigms, especially imperative, object-oriented, logic, and functional approaches.

## 6. Recording Script Version (Speak This)
*You can read this directly in your video:*

> "Our project is a Multi-Paradigm Expression Interpreter that evaluates arithmetic, relational, and boolean expressions.
>
> The same interpreter is implemented using four programming paradigms: Imperative using C, Object-Oriented using Python, Logic using Prolog, and Functional using Haskell.
>
> All implementations follow the same grammar and produce the same correct output.
>
> The purpose of the project is to compare how each programming paradigm approaches the same problem differently, highlighting their strengths, weaknesses, and design philosophies."

## 7. Why This Section Matters to Your Grade
This part:
*   Clearly defines what the project does
*   Establishes paradigm comparison as the main goal
*   Shows conceptual understanding, not just coding
*   Sets up everything that follows logically

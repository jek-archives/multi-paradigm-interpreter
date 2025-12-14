# Development Process: Multi-Paradigm Interpreter

**Author:** [Your Name/Team Name]  
**Date:** December 14, 2025

---

## 1. Project Vision & Architecture

The core objective of this project was not just to build four calculators, but to build **one language** interpreted by **four different paradigms**. This required a strict adherence to a "Unified Grammar" while respecting the idiomatic constraints of each language.

### The Unified Grammar
We defined a single EBNF grammar to ensure consistency. Whether you are in C (Imperative) or Prolog (Logic), the syntax `10 + 20 * 3` or `x = 10` functions identically.

*   **Decision**: We chose a recursive-descent compatible grammar to simplify implementation across all languages.
*   **Constraint**: We enforced standard operator precedence (PEMDAS) and variable state across all interpreters.

---

## 2. Implementation Journey by Paradigm

### A. Python (Object-Oriented Reference)
We started with Python to establish the baseline logic.
*   **Architecture**: We used the **Visitor Pattern**, a classic OOP design pattern for AST traversal.
    *   `Node` classes (`BinOp`, `Number`, `Assign`) represent the data.
    *   `Interpreter` class contains `visit_Node` methods to execute logic.
*   **Key Decision**: Python was used as the "Reference Implementation". If another interpreter's output disagreed with Python, the other interpreter was considered incorrect.

### B. C (Imperative & Low-Level)
The C implementation focused on raw performance and memory management.
*   **Architecture**: We used `structs` and tagged unions (`enum TokenType`) to build the AST.
*   **Challenge**: C has no automatic garbage collection.
    *   *Solution*: We implemented a `free_ast()` recursive function to manually release memory after every execution loop.
*   **Variable Support**: Unlike Python's dict, C required a custom **Linked List Environment** to store variables (`char* name`, `double value`) dynamically.

### C. Prolog (Logic Programming)
Prolog offered the most radical shift in thinking.
*   **Architecture**: Instead of "functions returning values", we used **predicates** and **unification**.
    *   `eval(Expr, Result)` became `eval(Expr, EnvIn, EnvOut, Result)`.
*   **Challenge**: Prolog is immutable. Variables cannot be reassigned (e.g., `X = 10`, `X = 20` fails).
    *   *Solution*: We implemented **State Threading**. The "Environment" is passed into a predicate, modified (a new list is created), and the *new* environment is passed out to the next step.
    *   *Parsing*: We leveraged Prolog's powerful **DCG (Definite Clause Grammars)** to parse tokens almost natively.

### D. Haskell (Functional Programming)
Haskell enforced strict type safety and purity.
*   **Architecture**: We used **Pattern Matching** on algebraic data types (`data Expr = ...`).
*   **Challenge**: Managing state (variables) in a pure functional language.
    *   *Solution*: Similar to Prolog, we threaded the state map (`Data.Map`) through the recursive evaluation calls.
    *   *Type Safety*: The compiler prevented entire classes of runtime errors (like null pointers) that we had to guard against in C.

---

## 3. Technical Challenges & Solutions

### The "Universal Variable" Problem
*   **Problem**: Python makes variables easy. C makes them hard (memory). Haskell/Prolog make them "impossible" (immutability).
*   **Solution**: We implemented the **Environment Pattern** universally.
    *   **Python**: `self.variables = {}`
    *   **C**: `struct Env { char* key; double val; struct Env* next; }`
    *   **Haskell**: `Map String Double` passed recursively.
    *   **Prolog**: `[(Key, Val) | Rest]` list passed recursively.

### The Web Integration & "Exit" Command
*   **Problem**: The project runs on a web interface (`gui/index.html`), but the interpreters are console applications.
*   **Initial Failure**: The web server would hang indefinitely because the interpreter never closed.
*   **Solution**:
    *   We enforced a strict `exit` command protocol.
    *   The web server listens for the user's code, runs the interpreter, and waits for the process to terminate.
    *   We updated all 4 interpreters to print a standardized `"Exited."` message only upon explicit request, allowing the web frontend to distinguish between a crash and a clean exit.

---

## 4. Conclusion
This project demonstrates that while the **syntax** (what the user types) remains constant, the **mechanisms** (how the computer processes it) vary wildly between paradigms. We successfully unified these disparate worlds into a single, cohesive user experience.

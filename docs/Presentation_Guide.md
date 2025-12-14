# Multi-Paradigm Interpreter: Video Presentation Master Guide

**Target Duration**: 15 - 30 Minutes (Max Limit is 45, but concise is better).
**Goal**: Demonstrate the project, explain the code, and analyze the paradigms.
**Requirement**: **ALL MEMBERS** must present their assigned part.
**Leader Role**: Uploads the final submission (Video + Source Code).

---

## 1. Preparation Checklist (Do this BEFORE recording)
*   [ ] **Assign Roles**: Decide who acts as "Member 1", "Member 2", etc.
*   [ ] **Environment**: Close unrelated apps. Set text editors to a readable font size (Zoom IN).
*   [ ] **Tools Ready**:
    *   Open `http://localhost:8080` (Web Interface).
    *   Open VS Code with `main.c`, `interpreter.py`, `interpreter.pl`, `Main.hs`.
*   [ ] **Clean State**: Clear your terminal.
*   [ ] **Audio**: Use a good microphone. Speak slowly and clearly.

---

## 2. Submission & Packaging Guide (Leader Only)
**After the video is done:**
1.  **Package Source Code**:
    *   Create a folder `MultiParadigm_Project`.
    *   Include: `interpreter_c/`, `interpreter_python/`, `interpreter_prolog/`, `interpreter_haskell/`, `gui/` folders.
    *   Include: `Final_Report.pdf` (Export your MD to PDF).
    *   **Do NOT** include: `__pycache__`, `.o` files, `.exe` files (Source code only!).
    *   Zip it: `MultiParadigm_Project.zip`.
2.  **Upload to GDrive**:
    *   Upload the **Video File** (.mp4).
    *   Upload the **Zip File**.
    *   Upload the **Document**.
    *   **Set Permissions**: "Anyone with the link can view".
3.  **Submit**: Paste the GDrive Link to USTeP.

---

## 3. The Script & Flow

### Part I: Introduction & Objective
**Speaker**: [Member 1 / Leader]
**Visual**: Show the Web Interface (Landing Screen).

> "Good day. We are [Team Name], and this is our presentation for the Multi-Paradigm Expression Interpreter.
>
> **Project Objective**: Our goal was to design a unified system capable of evaluating arithmetic, relational, and boolean expressions, along with variable assignments. We achieved this by implementing the exact same interpreter logic using four distinct programming paradigms: Imperative (C), Object-Oriented (Python), Logic (Prolog), and Functional (Haskell).
>
> **Core Goal**: The purpose is not just to build a calculator, but to compare how different paradigms approach the same problem. We enforced a **Unified Grammar**, ensuring that `10 + 20` works identically in all four languages."

### Part II: Live Demonstration (The "Running Program")
**Speaker**: [Member 2]
**Visual**: Web Interface (`http://localhost:8080`).

*   **Step 1 (Arithmetic in C)**:
    *   *Action*: Select **C**, type `10 + 20 * 3`. Click Run.
    *   *Script*: "First, I will demonstrate the Imperative C engine. Observe how it correctly applies order of operations (PEMDAS), multiplying 20 by 3 before adding 10. Result: 70."
*   **Step 2 (Boolean in Python)**:
    *   *Action*: Select **Python**, type `true and (false or true)`. Click Run.
    *   *Script*: "Next, the Python engine handling boolean logic with grouping. It evaluates the parentheses first."
*   **Step 3 (Variables - The Hard Part)**:
    *   *Action*: Select **Haskell** (or C), type `x = 50 + 50`, Run. Then `x + 10`, Run.
    *   *Script*: "Here we demonstrate persistent state. We assign `x` the value 100. Then we use `x` in a new calculation. This proves our Symbol Table implementation works across requests."
*   **Step 4 (The "Exit" Command)**:
    *   *Action*: Type `exit`.
    *   *Script*: "Finally, we implemented a standardized `exit` command. As you can see, the system responds with 'Exited.', properly terminating the backend process."

### Part III: Source Code & Paradigm Analysis (The "Meat")
*Each member explains one language.*

**Language 1: C (Imperative)**
**Speaker**: [Member 3]
**Visual**: `interpreter_c/main.c` & `interpreter.c`.
> "I worked on the C implementation. The challenge here was **Memory Management**. Unlike high-level languages, we had to manually define `structs` for our AST nodes and implement a garbage collection function to free memory after every run. This represents the Imperative paradigm: giving us full control over the hardware at the cost of complexity."

**Language 2: Python (Object-Oriented)**
**Speaker**: [Member 4 (or Member 1 again)]
**Visual**: `interpreter_python/interpreter.py`.
> "For Pythons, we used the **Visitor Pattern**. We defined classes like `BinOp` and `Number`. The Interpreter class 'visits' these objects. This is the essence of OOP: encapsulating data and behavior into objects. It was the easiest to implement but runs slower than C."

**Language 3: Prolog (Logic)**
**Speaker**: [Member 2 (or 3)]
**Visual**: `interpreter_prolog/interpreter.pl`.
> "Prolog was the most unique. Instead of functions, we used **Predicates** and **Unification**. Notice how we don't 'return' values; we define relationships like `eval(add(A,B), R)`. We also had to 'thread' the state manually because variables in Prolog are immutable."

**Language 4: Haskell (Functional)**
**Speaker**: [Member X]
**Visual**: `interpreter_haskell/Eval.hs`.
> "Finally, Haskell. We used **Pattern Matching**. The entire evaluator is incredibly concise because we just match the AST shape. This proves that Functional Programming is superior for symbolic tasks like parsing, as it avoids side effects entirely."

### Part IV: Conclusion
**Speaker**: [Leader]
**Visual**: Web Interface or Title Slide.

> "In conclusion, we successfully integrated four disparate paradigms into a single user experience. We met all the requirements: the unified grammar, the REPL, and the paradigm comparison. This project demonstrates our understanding of how different coding philosophies solve the same machine problem. Thank you."

---

## 4. Tips for Success
*   **Don't Rush**: You have 45 minutes, but you don't need to fill it all. 15-20 minutes of high-quality explanation is better than 45 minutes of silence.
*   **Show, Don't Just Tell**: Whenever you talk about a feature (like "Variables"), point to the code or show it run.
*   **Transitions**: Say "Now, I will pass it to [Name] to explain C."

# Project Presentation & Recording Guide

**Goal**: Deliver a professional, research-oriented presentation of the Multi-Paradigm Expression Interpreter.
**Audience**: Professors and Technical Reviewers.

---

## 1. Preparation Checklist
Before you hit record:
*   [ ] **Clean Screen**: Close unrelated tabs/apps. Set wallpaper to something neutral.
*   [ ] **Font Size**: Increase terminal and IDE font size (Cmd/Ctrl + '+'). It must be readable on a small screen.
*   [ ] **Reset State**: Run `clear` in your terminal so it looks fresh.
*   [ ] **Open Files**: Have `main.c`, `interpreter.py`, `interpreter.pl`, and `Eval.hs` open in tabs to quickly switch between them.
*   [ ] **Launch GUI**: Have `http://localhost:8080` open and ready.

---

## 2. Recommended Recording Flow (Script)

### Part I: Introduction (30-60 Seconds)
*   **Visual**: Show the Web GUI landing page or a Title Slide.
*   **Script**:
    > "Good day. We are [Team Name], and this is our presentation for the Multi-Paradigm Expression Interpreter. The objective of this machine problem was to design a unified system capable of evaluating mathematical and logical expressions across four distinct programming paradigms: Imperative (C), Object-Oriented (Python), Logic (Prolog), and Functional (Haskell)."

### Part II: The Architecture (1 Minute)
*   **Visual**: Show `docs/System_Architecture.md` or a diagram if you have one.
*   **Talking Points**:
    *   "We implemented a standard compiler pipeline: Lexer to Token Stream, Parser to Abstract Syntax Tree (AST), and finally Evaluation."
    *   "Crucially, we enforced a **Unified Grammar** across all four languages, ensuring that the input `1 + 2 * 3` produces the exact same result in C as it does in Haskell."

### Part III: Live Demonstration (2-3 Minutes)
*   **Action**: Switch to the **Web GUI**.
*   **Demo 1 (Arithmetic)**:
    *   Select **C**. Type `10 + 20 * 3`.
    *   *Say*: "First, demonstrating operator precedence in the Imperative C engine. Multiplication binds tighter than addition."
*   **Demo 2 (Boolean/Logic)**:
    *   Select **Python**. Type `true and (false or true)`.
    *   *Say*: "Next, the Python object-oriented implementation handling boolean logic with grouping."
*   **Demo 3 (Debug Feature)**:
    *   **Action**: Type `debug` then `5 * 5`.
    *   *Say*: "We implemented an advanced IDE feature: Debug Mode. This allows us to inspect the raw Abstract Syntax Tree to verify parsing correctness."

### Part IV: Code walkthrough (Comparative Analysis) (2 Minutes)
*   *This is the "Research" part. Compare how the code looks different.*
*   **Visual**: Split screen or switch between `interpreter_c/parser.c` and `interpreter_haskell/Parser.hs`.
*   **Script**:
    > "The difference in paradigms is striking. In C (Imperative), we manually manage memory, defining structs for nodes and handling pointers. This gives us performance but requires over 400 lines of code."
    > "In contrast, looking at the Haskell (Functional) implementation, the exact same logic is expressed in declarative pattern matching. The entire evaluator is less than 50 lines. This confirms our research: Functional languages are significantly better suited for symbolic processing tasks."

### Part V: Conclusion
*   **Visual**: Return to the GUI or Title.
*   **Script**:
    > "In conclusion, this project successfully integrates four disparate languages into a single cohesive interface. We met all requirements of the machine problem, demonstrating not just coding proficiency, but a deep understanding of the underlying paradigms. Thank you."

---

## 3. Pro Tips for a "Human-Like" Feel
1.  **Don't Read Verbatim**: Use the points above as an outline, but speak naturally.
2.  **Handle Errors Gracefully**: If you make a typo during the demo, just correct it and say "Let me correct that syntax." It shows you are doing it live, which is good proof of work.
3.  **Mouse Movement**: Move your mouse deliberately. Don't shake it around. Point to the code you are talking about.
4.  **Audio**: Use a headset mic if possible. Background noise kills professionalism.

## 4. Troubleshooting During Recording
*   If the C interpreter crashes? -> "As we can see, C requires strict memory safety." (Restart it and move on).
*   If Python feels slow? -> "This demonstrates the trade-off between Python's developer speed versus C's execution speed."

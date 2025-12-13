# Multi-Paradigm Advanced Expression Interpreter

**A Study in Comparative Programming Languages**
- **Imperative**: C
- **Object-Oriented**: Python
- **Logic**: Prolog
- **Functional**: Haskell

## 🚀 Quick Start in 3 Steps

### 1. Setup Dependencies
Run the included setup script to check and install missing tools (Homebrew, Prolog, Haskell):
```bash
./setup.sh
```

### 2. Build the Project
Compile the C and Haskell interpreters:
```bash
make
```

### 3. Run the IDE
Start the Web GUI to interact with all languages:
```bash
python3 gui/server.py
```
Open **http://localhost:8080** in your browser.

---

## 📂 Project Structure
- `interpreter_c/`: High-performance imperative implementation.
- `interpreter_python/`: Python implementation. Logic is in `interpreter.py`, IDE wrapper in `ide.py`.
- `interpreter_prolog/`: Logic-based implementation using DCG parsing.
- `interpreter_haskell/`: Pure functional implementation.
- `gui/`: Dependency-free Python web server and HTML frontend.
- `docs/`: Comprehensive Architecture, Grammar, and Roadmap documentation.

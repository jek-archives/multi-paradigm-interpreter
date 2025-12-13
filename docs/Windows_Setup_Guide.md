# 🪟 How to Run on Windows

If you move this project to a Windows computer, follow these exact steps.

## Step 1: Install Dependencies (The "Prerequisites")
You need to install 4 things.

1.  **Python**:
    *   Download from [python.org](https://www.python.org/downloads/).
    *   **IMPORTANT**: Check the box **"Add Python to PATH"** during installation.

2.  **GCC Compiler** (for C):
    *   Download [MinGW-w64](https://www.mingw-w64.org/) or [TDM-GCC](https://jmeubank.github.io/tdm-gcc/).
    *   Verify by typing `gcc --version` in PowerShell.

3.  **SWI-Prolog** (for Prolog):
    *   Download from [swi-prolog.org](https://www.swi-prolog.org/Download.html).
    *   **IMPORTANT**: Choose "Add swipl to the system PATH" during install.

4.  **Haskell (GHC)**:
    *   Open PowerShell as Admin.
    *   Run: `Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }`
    *   (Or just search "GHCup Windows" and run their installer).

---

## Step 2: Compile the Interpreters
Windows doesn't always have `make`. You can compile manually in PowerShell:

### 1. Compile C
```powershell
cd interpreter_c
gcc main.c lexer.c parser.c ast.c interpreter.c -o interpreter_c.exe
cd ..
```

### 2. Compile Haskell
```powershell
cd interpreter_haskell
ghc Main.hs -o Interpreter.exe
cd ..
```

*(Note: Prolog and Python don't need compiling).*

---

## Step 3: Run the GUI
1.  Open PowerShell in the main project folder.
2.  Run:
    ```powershell
    python gui/server.py
    ```
3.  Open browser to `http://localhost:8080`.

## Troubleshooting Windows
*   **Error**: "'swipl' is not recognized" -> You didn't add it to PATH. Reinstall Prolog and check the box.
*   **Error**: "make not found" -> Use the manual compile commands above.

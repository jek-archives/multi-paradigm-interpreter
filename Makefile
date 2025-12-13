# Master Makefile for Multi-Paradigm Interpreter

# Compilers
CC = gcc
PY = python3
PL = swipl
HS = ghc

# Flags
CFLAGS = -Wall -Wextra -g

# Directories
DIR_C = interpreter_c
DIR_PY = interpreter_python
DIR_PL = interpreter_prolog
DIR_HS = interpreter_haskell

# Targets
TARGET_C = $(DIR_C)/interpreter_c
TARGET_HS = $(DIR_HS)/Interpreter

.PHONY: all clean run_c run_py run_pl run_hs

# Build all compiled languages (C, Haskell)
all: build_c build_hs

# --- C Implementation ---
build_c: $(TARGET_C)

# Added ast.c to the dependencies and compile command
$(TARGET_C): $(DIR_C)/main.c $(DIR_C)/lexer.c $(DIR_C)/parser.c $(DIR_C)/interpreter.c $(DIR_C)/ast.c
	$(CC) $(CFLAGS) -o $@ $^

run_c: build_c
	./$(TARGET_C)

# --- Python Implementation ---
run_py:
	$(PY) $(DIR_PY)/ide.py

# --- Prolog Implementation ---
run_pl:
	@which $(PL) > /dev/null || (echo "Error: SWI-Prolog ($(PL)) not found. Install it via 'brew install swi-prolog'" && exit 1)
	$(PL) -s $(DIR_PL)/interpreter.pl -g repl -t halt

# --- Haskell Implementation ---
build_hs:
	@which $(HS) > /dev/null || (echo "Warning: GHC ($(HS)) not found. Skipping Haskell build. Install via 'brew install ghc'" && exit 0)
	@if [ -x "$$(command -v $(HS))" ]; then \
		$(HS) -o $(TARGET_HS) $(DIR_HS)/Main.hs $(DIR_HS)/Lexer.hs $(DIR_HS)/Parser.hs $(DIR_HS)/Eval.hs $(DIR_HS)/AST.hs; \
	fi

run_hs: build_hs
	@if [ -f "$(TARGET_HS)" ]; then \
		./$(TARGET_HS); \
	else \
		echo "Error: Haskell binary not found. Is GHC installed?"; \
	fi

# Cleanup
clean:
	rm -f $(TARGET_C) $(TARGET_HS) $(DIR_HS)/*.hi $(DIR_HS)/*.o

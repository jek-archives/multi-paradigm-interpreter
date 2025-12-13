# Fast Completion Checklist

## Phase 1: Setup (Day 1)
- [x] Download/Clone the scaffold.
- [x] Check if GCC, Python 3, SWI-Prolog, and GHC (Haskell) are installed. (Partial: GHC installing)
- [x] Verify `make` or build scripts work (create basic Makefiles if needed).

## Phase 2: Lexer (Days 2-3)
- [x] C: Fill in `next_token` in `lexer.c` to handle multi-digit numbers and keywords.
- [x] Python: Enhance `lexer.py` regex or loop to capture `and`, `or`, etc.
- [x] Prolog: Complete the `lex` rules in `lexer.pl`.
- [x] Haskell: Add pattern matching for all tokens in `Lexer.hs`.

## Phase 3: Parser (Days 4-6)
- [x] C: Implement `parse_term` and `parse_expr` fully in `parser.c`.
- [x] Python: Finish `expr` and `term` methods in `parser.py`.
- [x] Prolog: Expand DCG rules in `parser.pl`.
- [x] Haskell: Complete recursive descent logic in `Parser.hs`.

## Phase 4: Eval (Days 7-9)
- [x] Implement `evaluate` function in all 4 languages.
- [x] Add `Symbol Table` support for variable assignments (`x = 10`).

## Phase 5: Final Polish
- [x] Test edge cases (division by zero).
- [x] Format code to be identical/consistent.
- [x] Write the Final PDF using `Documentation_Kit.md`.

## Bonus: IDE & Presentation
- [x] Web GUI with Dark Mode and Preset Examples.
- [x] Debug Mode (C, Python, Prolog).
- [x] Presentation Guide created.

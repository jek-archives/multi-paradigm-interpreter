:- use_module(lexer).
:- use_module(parser).

% Evaluator Rules
evaluate(num(N), N).
evaluate(bool(true), true).
evaluate(bool(false), false).

% Unary
evaluate(unaryOp(minus, Operand), Val) :- evaluate(Operand, V), Val is -V.
evaluate(unaryOp(not, Operand), Val) :- 
    evaluate(Operand, V),
    (V == true -> Val = false ; Val = true).

% Binary - Arithmetic
evaluate(binOp(plus, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), Val is LV + RV.
evaluate(binOp(minus, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), Val is LV - RV.
evaluate(binOp(times, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), Val is LV * RV.
evaluate(binOp(div, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), Val is LV / RV.
evaluate(binOp(mod, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), Val is mod(round(LV), round(RV)).

% Binary - Relational (Prolog usually returns true/false via success/failure, but here we return atomic 'true'/'false' values)
evaluate(binOp(lt, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), (LV < RV -> Val = true ; Val = false).
evaluate(binOp(gt, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), (LV > RV -> Val = true ; Val = false).
evaluate(binOp(le, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), (LV =< RV -> Val = true ; Val = false).
evaluate(binOp(ge, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), (LV >= RV -> Val = true ; Val = false).
evaluate(binOp(eq, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), (LV =:= RV -> Val = true ; Val = false). % Numeric equality
evaluate(binOp(neq, L, R), Val) :- evaluate(L, LV), evaluate(R, RV), (LV =\= RV -> Val = true ; Val = false).

% Binary - Logical
evaluate(binOp(and, L, R), Val) :- 
    evaluate(L, LV), evaluate(R, RV),
    (LV == true, RV == true -> Val = true ; Val = false).

evaluate(binOp(or, L, R), Val) :- 
    evaluate(L, LV), evaluate(R, RV),
    (LV == true ; RV == true -> Val = true ; Val = false).


% Dynamic flag for debug mode
:- dynamic debug_mode/0.

% REPL Loop
repl :-
    write('>>> '),
    read_line_to_string(user_input, Input),
    (Input == "exit" -> writeln('Goodbye.');
     Input == "debug" -> 
        (debug_mode -> retract(debug_mode), writeln('Debug Mode: OFF');
                       assert(debug_mode), writeln('Debug Mode: ON')),
        repl;
     run(Input), repl).

run(Input) :-
    catch(
        (tokenize(Input, Tokens),
         parse(Tokens, AST),
         (debug_mode -> format('Tokens: ~w~nAST: ~w~n', [Tokens, AST]); true),
         evaluate(AST, Result),
         format('Result: ~w~n', [Result])),
        E,
        format('Error: ~w~n', [E])
    ).

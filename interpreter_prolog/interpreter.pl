:- use_module(lexer).
:- use_module(parser).

% Evaluator Rules
% Evaluator Rules - Now with Environment (Threading State)
% evaluate(Node, EnvIn, EnvOut, Value)

evaluate(num(N), Env, Env, N).
evaluate(bool(true), Env, Env, true).
evaluate(bool(false), Env, Env, false).

evaluate(assign(Name, Expr), EnvIn, EnvOut, Val) :-
    evaluate(Expr, EnvIn, EnvMid, Val),
    EnvOut = [Name-Val | EnvMid].

evaluate(var(Name), Env, Env, Val) :-
    member(Name-Val, Env), !.
evaluate(var(Name), _, _, _) :-
    format('Error: Undefined variable ~w~n', [Name]), fail.

% Unary
% Unary
evaluate(unaryOp(minus, Operand), EnvIn, EnvOut, Val) :- 
    evaluate(Operand, EnvIn, EnvOut, V), Val is -V.
evaluate(unaryOp(not, Operand), EnvIn, EnvOut, Val) :- 
    evaluate(Operand, EnvIn, EnvOut, V),
    (V == true -> Val = false ; Val = true).

% Binary - Arithmetic
% Binary - Arithmetic
evaluate(binOp(plus, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), Val is LV + RV.
evaluate(binOp(minus, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), Val is LV - RV.
evaluate(binOp(times, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), Val is LV * RV.
evaluate(binOp(div, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), Val is LV / RV.
evaluate(binOp(mod, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), Val is mod(round(LV), round(RV)).

% Binary - Relational
evaluate(binOp(lt, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), (LV < RV -> Val = true ; Val = false).
evaluate(binOp(gt, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), (LV > RV -> Val = true ; Val = false).
evaluate(binOp(le, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), (LV =< RV -> Val = true ; Val = false).
evaluate(binOp(ge, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), (LV >= RV -> Val = true ; Val = false).
evaluate(binOp(eq, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), (LV =:= RV -> Val = true ; Val = false).
evaluate(binOp(neq, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV), (LV =\= RV -> Val = true ; Val = false).

% Binary - Logical
% Binary - Logical
evaluate(binOp(and, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV),
    (LV == true, RV == true -> Val = true ; Val = false).

evaluate(binOp(or, L, R), EnvIn, EnvOut, Val) :- 
    evaluate(L, EnvIn, EnvMid, LV), evaluate(R, EnvMid, EnvOut, RV),
    (LV == true ; RV == true -> Val = true ; Val = false).


% Dynamic flag for debug mode
:- dynamic debug_mode/0.

% Entry Point
% Entry Point
start :-
    writeln('Prolog Interpreter REPL.'),
    writeln('Commands: ''debug'' to toggle AST view, ''exit'' to quit.'),
    repl([]).

% REPL Loop
repl(Env) :-
    write('>>> '),
    read_line_to_string(user_input, Input),
    (Input == "exit" -> writeln('Exited.');
     Input == end_of_file -> true;
     Input == "debug" -> 
        (debug_mode -> retract(debug_mode), writeln('Debug Mode: OFF');
                       assert(debug_mode), writeln('Debug Mode: ON')),
        repl(Env);
     run(Input, Env, NewEnv), repl(NewEnv)).

run(Input, EnvIn, EnvOut) :-
    catch(
        (tokenize(Input, Tokens),
         parse(Tokens, AST),
         (debug_mode -> format('Tokens: ~w~nAST: ~w~n', [Tokens, AST]); true),
         evaluate(AST, EnvIn, EnvOut, Result),
         format('Result: ~w~n', [Result])),
        E,
        (format('Error: ~w~n', [E]), EnvOut = EnvIn)
    ).

:- module(lexer, [tokenize/2]).

% Entry point
tokenize(String, Tokens) :-
    string_chars(String, Chars),
    lex(Chars, Tokens).

% Base case
lex([], []).
lex([C | T], Tokens) :- char_type(C, space), lex(T, Tokens). % Skip whitespace

% Multi-character Operators (Try these first!)
lex(['=', '=' | T], [eq | Rest]) :- lex(T, Rest).
lex(['!', '=' | T], [neq | Rest]) :- lex(T, Rest).
lex(['<', '=' | T], [le | Rest]) :- lex(T, Rest).
lex(['>', '=' | T], [ge | Rest]) :- lex(T, Rest).

% Single-character Operators
lex(['+' | T], [plus | Rest]) :- lex(T, Rest).
lex(['-' | T], [minus | Rest]) :- lex(T, Rest).
lex(['*' | T], [times | Rest]) :- lex(T, Rest).
lex(['/' | T], [div | Rest]) :- lex(T, Rest).
lex(['%' | T], [mod | Rest]) :- lex(T, Rest).
lex(['(' | T], [lparen | Rest]) :- lex(T, Rest).
lex([')' | T], [rparen | Rest]) :- lex(T, Rest).
lex(['=' | T], [assign | Rest]) :- lex(T, Rest).
lex(['<' | T], [lt | Rest]) :- lex(T, Rest).
lex(['>' | T], [gt | Rest]) :- lex(T, Rest).

% Numbers (Digits + Optional Dot)
lex([C | T], [num(N) | Rest]) :-
    char_type(C, digit),
    read_number([C | T], NumChars, NextChars),
    number_chars(N, NumChars),
    lex(NextChars, Rest).

% Identifiers and Keywords
lex([C | T], [Token | Rest]) :-
    char_type(C, alpha),
    read_ident([C | T], IdChars, NextChars),
    atom_chars(Atom, IdChars),
    classify_ident(Atom, Token),
    lex(NextChars, Rest).

% Error Handling
lex([C | _], _) :- 
    format('Lexical error: unexpected character ~w~n', [C]), fail.

% Helpers
read_number([C | T], [C | NumT], Rest) :-
    (char_type(C, digit) ; C == '.'),
    read_number(T, NumT, Rest).
read_number(Rest, [], Rest).

read_ident([C | T], [C | IdT], Rest) :-
    (char_type(C, alnum) ; C == '_'),
    read_ident(T, IdT, Rest).
read_ident(Rest, [], Rest).

% Keyword Classification
classify_ident(and, and).
classify_ident(or, or).
classify_ident(not, not).
classify_ident(true, bool(true)).
classify_ident(false, bool(false)).
classify_ident(Id, ident(Id)).

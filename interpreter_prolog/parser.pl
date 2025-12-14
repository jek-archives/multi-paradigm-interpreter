:- module(parser, [parse/2]).
:- use_module(lexer).

% Grammar Rules (DCG)
parse(Tokens, AST) :-
    expression(AST, Tokens, []).

% Hierarchy: logical_or -> logical_and -> equality -> relational -> additive -> multiplicative -> unary -> primary

% Hierarchy: logical_or -> logical_and -> equality -> relational -> additive -> multiplicative -> unary -> primary

expression(assign(Name, Expr)) --> [ident(Name)], [assign], expression(Expr).
expression(AST) --> logical_or(AST).

% Logical OR
logical_or(AST) --> logical_and(Left), logical_or_rest(Left, AST).

logical_or_rest(Acc, Final) --> 
    [or], logical_and(Right), 
    { NewAcc = binOp(or, Acc, Right) },
    logical_or_rest(NewAcc, Final).
logical_or_rest(Acc, Acc) --> [].

% Logical AND
logical_and(AST) --> equality(Left), logical_and_rest(Left, AST).

logical_and_rest(Acc, Final) --> 
    [and], equality(Right), 
    { NewAcc = binOp(and, Acc, Right) },
    logical_and_rest(NewAcc, Final).
logical_and_rest(Acc, Acc) --> [].

% Equality
equality(AST) --> relational(Left), equality_rest(Left, AST).

equality_rest(Acc, Final) --> 
    [eq], relational(Right), 
    { NewAcc = binOp(eq, Acc, Right) },
    equality_rest(NewAcc, Final).
equality_rest(Acc, Final) --> 
    [neq], relational(Right), 
    { NewAcc = binOp(neq, Acc, Right) },
    equality_rest(NewAcc, Final).
equality_rest(Acc, Acc) --> [].

% Relational
relational(AST) --> additive(Left), relational_rest(Left, AST).

relational_rest(Acc, Final) --> 
    [lt], additive(Right), 
    { NewAcc = binOp(lt, Acc, Right) },
    relational_rest(NewAcc, Final).
relational_rest(Acc, Final) --> 
    [gt], additive(Right), 
    { NewAcc = binOp(gt, Acc, Right) },
    relational_rest(NewAcc, Final).
relational_rest(Acc, Final) --> 
    [le], additive(Right), 
    { NewAcc = binOp(le, Acc, Right) },
    relational_rest(NewAcc, Final).
relational_rest(Acc, Final) --> 
    [ge], additive(Right), 
    { NewAcc = binOp(ge, Acc, Right) },
    relational_rest(NewAcc, Final).
relational_rest(Acc, Acc) --> [].

% Additive
additive(AST) --> multiplicative(Left), additive_rest(Left, AST).

additive_rest(Acc, Final) --> 
    [plus], multiplicative(Right), 
    { NewAcc = binOp(plus, Acc, Right) },
    additive_rest(NewAcc, Final).
additive_rest(Acc, Final) --> 
    [minus], multiplicative(Right), 
    { NewAcc = binOp(minus, Acc, Right) },
    additive_rest(NewAcc, Final).
additive_rest(Acc, Acc) --> [].

% Multiplicative
multiplicative(AST) --> unary(Left), multiplicative_rest(Left, AST).

multiplicative_rest(Acc, Final) --> 
    [times], unary(Right), 
    { NewAcc = binOp(times, Acc, Right) },
    multiplicative_rest(NewAcc, Final).
multiplicative_rest(Acc, Final) --> 
    [div], unary(Right), 
    { NewAcc = binOp(div, Acc, Right) },
    multiplicative_rest(NewAcc, Final).
multiplicative_rest(Acc, Final) --> 
    [mod], unary(Right), 
    { NewAcc = binOp(mod, Acc, Right) },
    multiplicative_rest(NewAcc, Final).
multiplicative_rest(Acc, Acc) --> [].

% Unary
unary(unaryOp(minus, Operand)) --> [minus], unary(Operand).
unary(unaryOp(not, Operand)) --> [not], unary(Operand).
unary(AST) --> primary(AST).

% Primary
primary(num(N)) --> [num(N)].
primary(bool(true)) --> [bool(true)].
primary(bool(false)) --> [bool(false)].
primary(Expr) --> [lparen], expression(Expr), [rparen].
primary(var(Name)) --> [ident(Name)].

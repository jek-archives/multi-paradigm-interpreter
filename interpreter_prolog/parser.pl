:- module(parser, [parse/2]).
:- use_module(lexer).

% Grammar Rules (DCG)
parse(Tokens, AST) :-
    expression(AST, Tokens, []).

% Hierarchy: logical_or -> logical_and -> equality -> relational -> additive -> multiplicative -> unary -> primary

expression(AST) --> logical_or(AST).

% Logical OR
logical_or(AST) --> logical_and(Left), logical_or_rest(Left, AST).
logical_or_rest(Left, binOp(or, Left, Right)) --> [or], logical_and(Right), logical_or_rest(Right, AST). % Incorrect recursion for Left assoc, but simple for now
logical_or_rest(Node, Node) --> [].

% Logical AND
logical_and(AST) --> equality(Left), logical_and_rest(Left, AST).
logical_and_rest(Left, binOp(and, Left, Right)) --> [and], equality(Right), logical_and_rest(Right, AST).
logical_and_rest(Node, Node) --> [].

% Equality
equality(AST) --> relational(Left), equality_rest(Left, AST).
equality_rest(Left, binOp(eq, Left, Right)) --> [eq], relational(Right), equality_rest(Right, AST).
equality_rest(Left, binOp(neq, Left, Right)) --> [neq], relational(Right), equality_rest(Right, AST).
equality_rest(Node, Node) --> [].

% Relational
relational(AST) --> additive(Left), relational_rest(Left, AST).
relational_rest(Left, binOp(lt, Left, Right)) --> [lt], additive(Right), relational_rest(Right, AST).
relational_rest(Left, binOp(gt, Left, Right)) --> [gt], additive(Right), relational_rest(Right, AST).
relational_rest(Left, binOp(le, Left, Right)) --> [le], additive(Right), relational_rest(Right, AST).
relational_rest(Left, binOp(ge, Left, Right)) --> [ge], additive(Right), relational_rest(Right, AST).
relational_rest(Node, Node) --> [].

% Additive
additive(AST) --> multiplicative(Left), additive_rest(Left, AST).
additive_rest(Left, binOp(plus, Left, Right)) --> [plus], multiplicative(Right), additive_rest(Right, AST). % Note: this is Right Associative in basic DCG. 
additive_rest(Left, binOp(minus, Left, Right)) --> [minus], multiplicative(Right), additive_rest(Right, AST).
additive_rest(Node, Node) --> [].

% Multiplicative
multiplicative(AST) --> unary(Left), multiplicative_rest(Left, AST).
multiplicative_rest(Left, binOp(times, Left, Right)) --> [times], unary(Right), multiplicative_rest(Right, AST).
multiplicative_rest(Left, binOp(div, Left, Right)) --> [div], unary(Right), multiplicative_rest(Right, AST).
multiplicative_rest(Left, binOp(mod, Left, Right)) --> [mod], unary(Right), multiplicative_rest(Right, AST).
multiplicative_rest(Node, Node) --> [].

% Unary
unary(unaryOp(minus, Operand)) --> [minus], unary(Operand).
unary(unaryOp(not, Operand)) --> [not], unary(Operand).
unary(AST) --> primary(AST).

% Primary
primary(num(N)) --> [num(N)].
primary(bool(true)) --> [bool(true)].
primary(bool(false)) --> [bool(false)].
primary(Expr) --> [lparen], expression(Expr), [rparen].

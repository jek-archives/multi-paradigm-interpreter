# Unified Grammar Specification

This grammar applies to all four implementations (C, Python, Prolog, Haskell).

## Supported Operations
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Boolean**: `and`, `or`, `not`
- **Relational**: `<`, `>`, `<=`, `>=` (Optional: `==`, `!=`)
- **Grouping**: `(`, `)`
- **Assignment**: `var = expr`

## EBNF (Extended Backus-Naur Form)

```ebnf
(* Entry Point *)
program         = { statement } ;
statement       = assignment | expression ;

(* Assignments *)
assignment      = identifier "=" expression ;

(* Expression Hierarchy (Precedence Levels) *)
expression      = logical_or ;

logical_or      = logical_and { "or" logical_and } ;
logical_and     = equality { "and" equality } ;
equality        = relational { ("==" | "!=") relational } ;
relational      = additive { ("<" | ">" | "<=" | ">=") additive } ;

additive        = multiplicative { ("+" | "-") multiplicative } ;
multiplicative  = unary { ("*" | "/" | "%") unary } ;

unary           = ("-" | "not") unary | primary ;

primary         = number
                | boolean
                | identifier
                | "(" expression ")" ;

(* Tokens *)
number          = digit { digit } [ "." digit { digit } ] ;
boolean         = "true" | "false" ;
identifier      = letter { letter | digit | "_" } ;
letter          = "a"..."z" | "A"..."Z" ;
digit           = "0"..."9" ;
```

## Operator Precedence (Highest to Lowest)
1. `()` (Grouping)
2. `not` `-` (Unary)
3. `*` `/` `%`
4. `+` `-`
5. `<` `>` `<=` `>=` `==` `!=`
6. `and`
7. `or`
8. `=` (Assignment)

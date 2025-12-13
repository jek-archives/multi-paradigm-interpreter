#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

typedef struct {
  Lexer lexer;
  Token current_token;
} Parser;

void init_parser(Parser *parser, const char *source);
ASTNode *parse_expression(Parser *parser);

#endif

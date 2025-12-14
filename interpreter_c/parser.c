#include "parser.h"
#include <stdio.h>
#include <stdlib.h>

void eat(Parser *parser, TokenType type) {
  if (parser->current_token.type == type) {
    free_token(parser->current_token);
    parser->current_token = parser->peek_token;
    parser->peek_token = next_token(&parser->lexer);
  } else {
    printf("Syntax Error: Expected %d, got %d\n", type,
           parser->current_token.type);
    exit(1);
  }
}

void init_parser(Parser *parser, const char *source) {
  init_lexer(&parser->lexer, source);
  parser->current_token = next_token(&parser->lexer);
  parser->peek_token = next_token(&parser->lexer);
}

// Forward declaration
ASTNode *parse_expression(Parser *parser);

ASTNode *parse_primary(Parser *parser) {
  Token token = parser->current_token;

  if (token.type == TOKEN_NUMBER) {
    eat(parser, TOKEN_NUMBER);
    return create_number_node(token.number);
  } else if (token.type == TOKEN_TRUE) {
    eat(parser, TOKEN_TRUE);
    return create_boolean_node(1);
  } else if (token.type == TOKEN_FALSE) {
    eat(parser, TOKEN_FALSE);
    return create_boolean_node(0);
  } else if (token.type == TOKEN_LPAREN) {
    eat(parser, TOKEN_LPAREN);
    ASTNode *node = parse_expression(parser);
    eat(parser, TOKEN_RPAREN);
    return node;
  } else if (token.type == TOKEN_IDENTIFIER) {
    char *name = strdup(token.text); 
    eat(parser, TOKEN_IDENTIFIER);
    ASTNode *node = create_identifier_node(name);
    free(name); 
    return node;
  }

  printf("Syntax Error: Unexpected token type %d\n", token.type);
  return NULL;
}

ASTNode *parse_unary(Parser *parser) {
  if (parser->current_token.type == TOKEN_PLUS) {
    eat(parser, TOKEN_PLUS);
    return create_unary_node(0, parse_unary(parser));
  } else if (parser->current_token.type == TOKEN_MINUS) {
    eat(parser, TOKEN_MINUS);
    return create_unary_node(0, parse_unary(parser)); // 0 means '-'
  } else if (parser->current_token.type == TOKEN_NOT) {
    eat(parser, TOKEN_NOT);
    return create_unary_node(1, parse_unary(parser)); // 1 means 'not'
  }
  return parse_primary(parser);
}

ASTNode *parse_multiplicative(Parser *parser) {
  ASTNode *node = parse_unary(parser);
  while (parser->current_token.type == TOKEN_STAR ||
         parser->current_token.type == TOKEN_SLASH ||
         parser->current_token.type == TOKEN_MOD) {

    TokenType type = parser->current_token.type;
    eat(parser, type);

    ASTNode *right = parse_unary(parser);
    OpType op = OP_MUL;
    if (type == TOKEN_SLASH)
      op = OP_DIV;
    if (type == TOKEN_MOD)
      op = OP_MOD;

    node = create_binary_node(op, node, right);
  }
  return node;
}

ASTNode *parse_additive(Parser *parser) {
  ASTNode *node = parse_multiplicative(parser);
  while (parser->current_token.type == TOKEN_PLUS ||
         parser->current_token.type == TOKEN_MINUS) {

    TokenType type = parser->current_token.type;
    eat(parser, type);

    ASTNode *right = parse_multiplicative(parser);
    OpType op = (type == TOKEN_PLUS) ? OP_ADD : OP_SUB;

    node = create_binary_node(op, node, right);
  }
  return node;
}

ASTNode *parse_relational(Parser *parser) {
  ASTNode *node = parse_additive(parser);
  while (parser->current_token.type == TOKEN_LT ||
         parser->current_token.type == TOKEN_GT ||
         parser->current_token.type == TOKEN_LE ||
         parser->current_token.type == TOKEN_GE) {

    TokenType type = parser->current_token.type;
    eat(parser, type);

    ASTNode *right = parse_additive(parser);
    OpType op;
    if (type == TOKEN_LT)
      op = OP_LT;
    else if (type == TOKEN_GT)
      op = OP_GT;
    else if (type == TOKEN_LE)
      op = OP_LE;
    else
      op = OP_GE;

    node = create_binary_node(op, node, right);
  }
  return node;
}

ASTNode *parse_equality(Parser *parser) {
  ASTNode *node = parse_relational(parser);
  while (parser->current_token.type == TOKEN_EQ ||
         parser->current_token.type == TOKEN_NE) {

    TokenType type = parser->current_token.type;
    eat(parser, type);

    ASTNode *right = parse_relational(parser);
    OpType op = (type == TOKEN_EQ) ? OP_EQ : OP_NE;

    node = create_binary_node(op, node, right);
  }
  return node;
}

ASTNode *parse_logical_and(Parser *parser) {
  ASTNode *node = parse_equality(parser);
  while (parser->current_token.type == TOKEN_AND) {
    eat(parser, TOKEN_AND);
    node = create_binary_node(OP_AND, node, parse_equality(parser));
  }
  return node;
}

ASTNode *parse_logical_or(Parser *parser) {
  ASTNode *node = parse_logical_and(parser);
  while (parser->current_token.type == TOKEN_OR) {
    eat(parser, TOKEN_OR);
    node = create_binary_node(OP_OR, node, parse_logical_and(parser));
  }
  return node;
}

ASTNode *parse_expression(Parser *parser) { 
  if (parser->current_token.type == TOKEN_IDENTIFIER && 
      parser->peek_token.type == TOKEN_ASSIGN) {
    
    char *name = strdup(parser->current_token.text);
    eat(parser, TOKEN_IDENTIFIER);
    eat(parser, TOKEN_ASSIGN);
    
    ASTNode *expr = parse_expression(parser); // Recursive for chained assignment or just expr
    ASTNode *node = create_assignment_node(name, expr);
    free(name);
    return node;
  }
  return parse_logical_or(parser); 
}

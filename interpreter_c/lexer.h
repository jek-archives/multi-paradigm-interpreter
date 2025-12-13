#ifndef LEXER_H
#define LEXER_H

typedef enum {
  TOKEN_EOF,
  TOKEN_NUMBER,
  TOKEN_IDENTIFIER,
  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_STAR,
  TOKEN_SLASH,
  TOKEN_MOD,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_ASSIGN,
  TOKEN_AND,
  TOKEN_OR,
  TOKEN_NOT,
  TOKEN_LT,
  TOKEN_GT,
  TOKEN_LE,
  TOKEN_GE,
  TOKEN_EQ,
  TOKEN_NE,
  TOKEN_TRUE,
  TOKEN_FALSE
} TokenType;

typedef struct {
  TokenType type;
  char *text;    // For identifiers or exact string representation
  double number; // For numeric tokens
} Token;

typedef struct {
  const char *source;
  int pos;
  int len;
} Lexer;

void init_lexer(Lexer *lexer, const char *source);
Token next_token(Lexer *lexer);
void free_token(Token token);
const char *token_type_to_string(TokenType type);

#endif

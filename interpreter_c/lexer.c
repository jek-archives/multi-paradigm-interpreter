#include "lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void init_lexer(Lexer *lexer, const char *source) {
  lexer->source = source;
  lexer->pos = 0;
  lexer->len = strlen(source);
}

// Helper to peek at the next character without advancing
char peek(Lexer *lexer) {
  if (lexer->pos + 1 >= lexer->len)
    return '\0';
  return lexer->source[lexer->pos + 1];
}

Token next_token(Lexer *lexer) {
  Token token;
  token.text = NULL;
  token.number = 0;

  // Skip whitespace
  while (lexer->pos < lexer->len && isspace(lexer->source[lexer->pos])) {
    lexer->pos++;
  }

  if (lexer->pos >= lexer->len) {
    token.type = TOKEN_EOF;
    return token;
  }

  char current = lexer->source[lexer->pos];

  // Numbers
  if (isdigit(current)) {
    token.type = TOKEN_NUMBER;
    int start = lexer->pos;
    while (lexer->pos < lexer->len && (isdigit(lexer->source[lexer->pos]) ||
                                       lexer->source[lexer->pos] == '.')) {
      lexer->pos++;
    }
    int length = lexer->pos - start;
    char *numStr = (char *)malloc(length + 1);
    strncpy(numStr, &lexer->source[start], length);
    numStr[length] = '\0';
    token.number = strtod(numStr, NULL);
    free(numStr);
    return token;
  }

  // Identifiers and Keywords
  if (isalpha(current) || current == '_') {
    int start = lexer->pos;
    while (lexer->pos < lexer->len && (isalnum(lexer->source[lexer->pos]) ||
                                       lexer->source[lexer->pos] == '_')) {
      lexer->pos++;
    }
    int length = lexer->pos - start;
    char *text = (char *)malloc(length + 1);
    strncpy(text, &lexer->source[start], length);
    text[length] = '\0';

    if (strcmp(text, "and") == 0)
      token.type = TOKEN_AND;
    else if (strcmp(text, "or") == 0)
      token.type = TOKEN_OR;
    else if (strcmp(text, "not") == 0)
      token.type = TOKEN_NOT;
    else if (strcmp(text, "true") == 0) {
      token.type = TOKEN_TRUE;
      token.number = 1;
    } else if (strcmp(text, "false") == 0) {
      token.type = TOKEN_FALSE;
      token.number = 0;
    } else {
      token.type = TOKEN_IDENTIFIER;
      token.text = text; // Owner of memory is now the token
      return token;      // Return immediately to avoid freeing text
    }
    free(text); // Free if it was a keyword
    return token;
  }

  // Operators and Punctuation
  lexer->pos++;
  switch (current) {
  case '+':
    token.type = TOKEN_PLUS;
    break;
  case '-':
    token.type = TOKEN_MINUS;
    break;
  case '*':
    token.type = TOKEN_STAR;
    break;
  case '/':
    token.type = TOKEN_SLASH;
    break;
  case '%':
    token.type = TOKEN_MOD;
    break;
  case '(':
    token.type = TOKEN_LPAREN;
    break;
  case ')':
    token.type = TOKEN_RPAREN;
    break;
  case '=':
    if (lexer->pos < lexer->len && lexer->source[lexer->pos] == '=') {
      lexer->pos++;
      token.type = TOKEN_EQ;
    } else {
      token.type = TOKEN_ASSIGN;
    }
    break;
  case '!':
    if (lexer->pos < lexer->len && lexer->source[lexer->pos] == '=') {
      lexer->pos++;
      token.type = TOKEN_NE;
    } else {
      printf("Error: Unexpected character '!'\n");
      exit(1);
    }
    break;
  case '<':
    if (lexer->pos < lexer->len && lexer->source[lexer->pos] == '=') {
      lexer->pos++;
      token.type = TOKEN_LE;
    } else {
      token.type = TOKEN_LT;
    }
    break;
  case '>':
    if (lexer->pos < lexer->len && lexer->source[lexer->pos] == '=') {
      lexer->pos++;
      token.type = TOKEN_GE;
    } else {
      token.type = TOKEN_GT;
    }
    break;
  default:
    printf("Error: Unexpected character '%c'\n", current);
    exit(1);
  }

  return token;
}

void free_token(Token token) {
  if (token.text) {
    free(token.text);
    token.text = NULL;
  }
}

const char *token_type_to_string(TokenType type) {
  switch (type) {
  case TOKEN_EOF:
    return "EOF";
  case TOKEN_NUMBER:
    return "NUMBER";
  case TOKEN_IDENTIFIER:
    return "IDENTIFIER";
  case TOKEN_PLUS:
    return "PLUS";
  case TOKEN_MINUS:
    return "MINUS";
  case TOKEN_STAR:
    return "STAR";
  case TOKEN_SLASH:
    return "SLASH";
  case TOKEN_MOD:
    return "MOD";
  case TOKEN_LPAREN:
    return "LPAREN";
  case TOKEN_RPAREN:
    return "RPAREN";
  case TOKEN_ASSIGN:
    return "ASSIGN";
  case TOKEN_AND:
    return "AND";
  case TOKEN_OR:
    return "OR";
  case TOKEN_NOT:
    return "NOT";
  case TOKEN_LT:
    return "LT";
  case TOKEN_GT:
    return "GT";
  case TOKEN_LE:
    return "LE";
  case TOKEN_GE:
    return "GE";
  case TOKEN_EQ:
    return "EQ";
  case TOKEN_NE:
    return "NE";
  case TOKEN_TRUE:
    return "TRUE";
  case TOKEN_FALSE:
    return "FALSE";
  default:
    return "UNKNOWN";
  }
}

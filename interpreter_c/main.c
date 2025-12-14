#include "ast.h"
#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declaration
// Forward declaration
typedef struct Environment Environment;
Environment *create_environment();
double evaluate(ASTNode *node, Environment *env);

// Debug helper to print AST type (simplified)
void print_ast_debug(ASTNode *node, int level) {
  if (!node)
    return;
  for (int i = 0; i < level; i++)
    printf("  ");

  switch (node->type) {
  case NODE_NUMBER:
    printf("NUMBER: %f\n", node->data.numberValue);
    break;
  case NODE_BOOLEAN:
    printf("BOOLEAN: %d\n", node->data.booleanValue);
    break;
  case NODE_BINARY_OP:
    printf("BINARY_OP: %d\n", node->data.binary.op);
    print_ast_debug(node->data.binary.left, level + 1);
    print_ast_debug(node->data.binary.right, level + 1);
    break;
  case NODE_UNARY_OP:
    printf("UNARY_OP (Not: %d)\n", node->data.unary.isNot);
    print_ast_debug(node->data.unary.operand, level + 1);
    break;
  default:
    printf("NODE Type %d\n", node->type);
  }
}

int main() {
  char buffer[256];
  int debug_mode = 0;
  Environment *env = create_environment();

  printf("C Interpreter REPL\n");
  printf("Commands: 'debug' to toggle AST view, 'exit' to quit.\n");

  while (1) {
    printf(">>> ");
    if (fgets(buffer, sizeof(buffer), stdin) == NULL)
      break;

    // Remove newline
    buffer[strcspn(buffer, "\n")] = 0;

    if (strlen(buffer) == 0)
      continue;

    if (strcmp(buffer, "exit") == 0) {
      printf("Exited.\n");
      break;
    }

    if (strcmp(buffer, "debug") == 0) {
      debug_mode = !debug_mode;
      printf("Debug Mode: %s\n", debug_mode ? "ON" : "OFF");
      continue;
    }

    Parser parser;
    init_parser(&parser, buffer);
    ASTNode *ast = parse_expression(&parser);

    if (ast) {
      if (debug_mode) {
        printf("--- AST Debug ---\n");
        print_ast_debug(ast, 0);
        printf("-----------------\n");
      }

      double result = evaluate(ast, env);
      printf("Result: %f\n", result);

      free_ast(ast);
    }
  }

  return 0;
}

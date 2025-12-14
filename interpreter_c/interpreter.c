#include "ast.h"
#include <stdio.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Environment Implementation (Linked List)
typedef struct EnvNode {
  char *name;
  double value;
  struct EnvNode *next;
} EnvNode;

typedef struct Environment {
  EnvNode *head;
} Environment;

Environment *create_environment() {
  Environment *env = (Environment *)malloc(sizeof(Environment));
  env->head = NULL;
  return env;
}

double env_get(Environment *env, char *name) {
  EnvNode *current = env->head;
  while (current != NULL) {
    if (strcmp(current->name, name) == 0) {
      return current->value;
    }
    current = current->next;
  }
  printf("Error: Undefined variable '%s'\n", name);
  return 0;
}

void env_set(Environment *env, char *name, double value) {
  EnvNode *current = env->head;
  while (current != NULL) {
    if (strcmp(current->name, name) == 0) {
      current->value = value;
      return;
    }
    current = current->next;
  }
  // Not found, create new
  EnvNode *node = (EnvNode *)malloc(sizeof(EnvNode));
  node->name = strdup(name);
  node->value = value;
  node->next = env->head;
  env->head = node;
}


double evaluate(ASTNode *node, Environment *env) {
  if (!node)
    return 0;

  switch (node->type) {
  case NODE_NUMBER:
    return node->data.numberValue;

  case NODE_BOOLEAN:
    return (double)node->data.booleanValue;
    
  case NODE_IDENTIFIER:
    return env_get(env, node->data.identifierName);
    
  case NODE_ASSIGNMENT: {
      double val = evaluate(node->data.assignment.expression, env);
      env_set(env, node->data.assignment.name, val);
      return val;
  }

  case NODE_UNARY_OP: {
    double val = evaluate(node->data.unary.operand, env);
    if (node->data.unary.isNot) {
      return !((int)val);
    } else {
      return -val;
    }
  }

  case NODE_BINARY_OP: {
    double left = evaluate(node->data.binary.left, env);
    double right = evaluate(node->data.binary.right, env);
    switch (node->data.binary.op) {
    case OP_ADD:
      return left + right;
    case OP_SUB:
      return left - right;
    case OP_MUL:
      return left * right;
    case OP_MOD:
      return (int)left % (int)right;
    case OP_DIV:
      if (right == 0) {
        printf("Error: Division by zero\n");
        return 0;
      }
      return left / right;

    // Relational
    case OP_LT:
      return left < right;
    case OP_GT:
      return left > right;
    case OP_LE:
      return left <= right;
    case OP_GE:
      return left >= right;
    case OP_EQ:
      return left == right;
    case OP_NE:
      return left != right;

    // Logical
    case OP_AND:
      return ((int)left) && ((int)right);
    case OP_OR:
      return ((int)left) || ((int)right);

    default:
      return 0;
    }
  }
  // TODO: Implement other node types
  default:
    return 0;
  }
}

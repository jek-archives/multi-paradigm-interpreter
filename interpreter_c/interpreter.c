#include "ast.h"
#include <stdio.h>

// Symbol Table (Simplified for starter)
// TODO: Implement a hash map or linked list for variable storage

double evaluate(ASTNode *node) {
  if (!node)
    return 0;

  switch (node->type) {
  case NODE_NUMBER:
    return node->data.numberValue;

  case NODE_BOOLEAN:
    return (double)node->data.booleanValue;

  case NODE_UNARY_OP: {
    double val = evaluate(node->data.unary.operand);
    if (node->data.unary.isNot) {
      return !((int)val);
    } else {
      return -val;
    }
  }

  case NODE_BINARY_OP: {
    double left = evaluate(node->data.binary.left);
    double right = evaluate(node->data.binary.right);
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

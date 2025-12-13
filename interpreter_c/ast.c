#include "ast.h"
#include <stdio.h>
#include <stdlib.h>

ASTNode *create_node(NodeType type) {
  ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
  if (!node) {
    fprintf(stderr, "Error: Memory allocation failed\n");
    exit(1);
  }
  node->type = type;
  return node;
}

ASTNode *create_number_node(double value) {
  ASTNode *node = create_node(NODE_NUMBER);
  node->data.numberValue = value;
  return node;
}

ASTNode *create_boolean_node(int value) {
  ASTNode *node = create_node(NODE_BOOLEAN);
  node->data.booleanValue = value;
  return node;
}

ASTNode *create_binary_node(OpType op, ASTNode *left, ASTNode *right) {
  ASTNode *node = create_node(NODE_BINARY_OP);
  node->data.binary.op = op;
  node->data.binary.left = left;
  node->data.binary.right = right;
  return node;
}

ASTNode *create_unary_node(int isNot, ASTNode *operand) {
  ASTNode *node = create_node(NODE_UNARY_OP);
  node->data.unary.isNot = isNot;
  node->data.unary.operand = operand;
  return node;
}

void free_ast(ASTNode *node) {
  if (!node)
    return;

  switch (node->type) {
  case NODE_BINARY_OP:
    free_ast(node->data.binary.left);
    free_ast(node->data.binary.right);
    break;
  case NODE_UNARY_OP:
    free_ast(node->data.unary.operand);
    break;
  case NODE_ASSIGNMENT:
    free(node->data.assignment.name);
    free_ast(node->data.assignment.expression);
    break;
  case NODE_IDENTIFIER:
    free(node->data.identifierName);
    break;
  default:
    break;
  }

  free(node);
}

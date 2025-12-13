#ifndef AST_H
#define AST_H

typedef enum {
  NODE_NUMBER,
  NODE_BOOLEAN,
  NODE_IDENTIFIER,
  NODE_BINARY_OP,
  NODE_UNARY_OP,
  NODE_ASSIGNMENT
} NodeType;

typedef enum {
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_AND,
  OP_OR,
  OP_LT,
  OP_GT,
  OP_LE,
  OP_GE,
  OP_EQ,
  OP_NE
} OpType;

typedef struct ASTNode {
  NodeType type;
  union {
    double numberValue;
    int booleanValue; // 0 or 1
    char *identifierName;
    struct {
      struct ASTNode *left;
      struct ASTNode *right;
      OpType op;
    } binary;
    struct {
      struct ASTNode *operand;
      int isNot; // 1 if 'not', 0 if '-' (unary minus)
    } unary;
    struct {
      char *name;
      struct ASTNode *expression;
    } assignment;
  } data;
} ASTNode;

ASTNode *create_number_node(double value);
ASTNode *create_boolean_node(int value);
ASTNode *create_binary_node(OpType op, ASTNode *left, ASTNode *right);
ASTNode *create_unary_node(int isNot, ASTNode *operand);
void free_ast(ASTNode *node);

#endif

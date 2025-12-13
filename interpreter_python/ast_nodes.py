from dataclasses import dataclass
from typing import Any, Optional

@dataclass
class ASTNode:
    pass

@dataclass
class Number(ASTNode):
    value: float

@dataclass
class Boolean(ASTNode):
    value: bool

@dataclass
class BinOp(ASTNode):
    left: ASTNode
    op: str
    right: ASTNode

@dataclass
class UnaryOp(ASTNode):
    op: str
    operand: ASTNode

@dataclass
class VarAssign(ASTNode):
    name: str
    value: ASTNode

@dataclass
class Variable(ASTNode):
    name: str

from ast_nodes import Number, Boolean, BinOp, UnaryOp, Variable, VarAssign

class Interpreter:
    def __init__(self):
        self.variables = {}

    def visit(self, node):
        if node is None:
            return None
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')

    def visit_Number(self, node):
        return node.value

    def visit_Boolean(self, node):
        return node.value
    
    def visit_Variable(self, node):
        var_name = node.name
        if var_name in self.variables:
            return self.variables[var_name]
        raise Exception(f"Undefined variable '{var_name}'")

    def visit_VarAssign(self, node):
        val = self.visit(node.value)
        self.variables[node.name] = val
        return val

    def visit_UnaryOp(self, node):
        val = self.visit(node.operand)
        if node.op == '+':
            return +val
        elif node.op == '-':
            return -val
        elif node.op == 'not' or node.op == 'NOT':
            return not val
        raise Exception(f"Unknown unary operator: {node.op}")

    def visit_BinOp(self, node):
        left = self.visit(node.left)
        right = self.visit(node.right)
        
        op = node.op
        
        # Arithmetic
        if op == '+': return left + right
        if op == '-': return left - right
        if op == '*': return left * right
        if op == '/': return left / right
        if op == '%': return left % right
        
        # Relational
        if op == '<': return left < right
        if op == '>': return left > right
        if op == '<=': return left <= right
        if op == '>=': return left >= right
        if op == '==' or op == '=': return left == right
        if op == '!=': return left != right
        
        # Logical
        if op == 'and': return left and right
        if op == 'or': return left or right
        
        raise Exception(f"Unknown binary operator: {op}")

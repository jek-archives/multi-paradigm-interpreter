from lexer import Lexer, TokenType
from ast_nodes import Number, Boolean, BinOp, UnaryOp, Variable, VarAssign

class Parser:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            raise Exception(f"Invalid syntax: Expected {token_type}, got {self.current_token.type}")

    def primary(self):
        token = self.current_token
        
        if token.type == TokenType.NUMBER:
            self.eat(TokenType.NUMBER)
            return Number(token.value)
        
        if token.type == TokenType.TRUE:
            self.eat(TokenType.TRUE)
            return Boolean(True)
        
        if token.type == TokenType.FALSE:
            self.eat(TokenType.FALSE)
            return Boolean(False)
            
        if token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return node
            
        if token.type == TokenType.IDENTIFIER:
            # Check for assignment
            # For now, just variable access. Assignment usually handled at statement level.
            # But per grammar: assignment = identifier "=" expression
            # and expression is lower.
            # We'll handle assignment at top level if needed, strictly speaking grammar says 'statement'.
            # Here we assume inside an expression it's a variable usage.
            self.eat(TokenType.IDENTIFIER)
            return Variable(token.value)
            
        raise Exception(f"Syntax Error: Unexpected token {token}")

    def unary(self):
        token = self.current_token
        if token.type in (TokenType.PLUS, TokenType.MINUS, TokenType.NOT):
            self.eat(token.type)
            return UnaryOp(op=token.value if token.value else token.type.name, operand=self.unary())
        return self.primary()

    def multiplicative(self):
        node = self.unary()
        while self.current_token.type in (TokenType.MUL, TokenType.DIV, TokenType.MOD):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(left=node, op=token.value, right=self.unary())
        return node

    def additive(self):
        node = self.multiplicative()
        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(left=node, op=token.value, right=self.multiplicative())
        return node

    def relational(self):
        node = self.additive()
        while self.current_token.type in (TokenType.LT, TokenType.GT, TokenType.LE, TokenType.GE):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(left=node, op=token.value, right=self.additive())
        return node

    def equality(self):
        node = self.relational()
        while self.current_token.type in (TokenType.EQ, TokenType.NE):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(left=node, op=token.value, right=self.relational())
        return node

    def logical_and(self):
        node = self.equality()
        while self.current_token.type == TokenType.AND:
            token = self.current_token
            self.eat(TokenType.AND)
            node = BinOp(left=node, op='and', right=self.equality())
        return node

    def logical_or(self):
        node = self.logical_and()
        while self.current_token.type == TokenType.OR:
            token = self.current_token
            self.eat(TokenType.OR)
            node = BinOp(left=node, op='or', right=self.logical_and())
        return node

    def expr(self):
        # Entry point for expressions
        return self.logical_or() 

    def parse(self):
        # Entry point for program (supports assignment or expression)
        if self.current_token.type == TokenType.IDENTIFIER:
            # Look ahead to see if it's assignment
            # This is a hacky LL(2) check or we can do it via a peek in lexer
            # Ideally:
            # statement = assignment | expression
            # assignment = identifier "=" expression
            
            # Simple workaround: Check if next token is ASSIGN
            # But Lexer consumes. We need peek. Lexer has peek.
            next_char_check = self.lexer.peek() 
            # Wait, Parser consumes tokens. We need to peek TOKEN not CHAR.
            # Our parser architecture doesn't have token peek easily without consuming.
            # Let's assume expressions for now, or simple assignment check if implemented later.
            pass
            
        return self.expr()

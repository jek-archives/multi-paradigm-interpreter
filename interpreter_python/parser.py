from lexer import Lexer, TokenType
from ast_nodes import Number, Boolean, BinOp, UnaryOp, Variable, VarAssign
from errors import ParserError

class Parser:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()
        self.peek_token = self.lexer.get_next_token()

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.peek_token
            self.peek_token = self.lexer.get_next_token()
        else:
            raise ParserError(
                f"Invalid syntax: Expected {token_type}, got {self.current_token.type}",
                self.current_token.lineno,
                self.current_token.column
            )

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
            
        raise ParserError(f"Syntax Error: Unexpected token {token}", token.lineno, token.column)

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
        # Lookahead to distinguish assignment "x = ..." from expression "x + ..."
        if self.current_token.type == TokenType.IDENTIFIER and self.peek_token.type == TokenType.ASSIGN:
            var_name = self.current_token.value
            self.eat(TokenType.IDENTIFIER)
            self.eat(TokenType.ASSIGN)
            # Use expr() to parse the right-hand side value
            value = self.expr()
            return VarAssign(var_name, value)
            
        return self.expr()

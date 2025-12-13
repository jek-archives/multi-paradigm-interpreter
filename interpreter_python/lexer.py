from enum import Enum, auto
from typing import Any, Optional

class TokenType(Enum):
    NUMBER = auto()
    PLUS = auto()
    MINUS = auto()
    MUL = auto()
    DIV = auto()
    MOD = auto()
    LPAREN = auto()
    RPAREN = auto()
    
    # Boolean / Logical
    AND = auto()
    OR = auto()
    NOT = auto()
    TRUE = auto()
    FALSE = auto()
    
    # Relational
    LT = auto()
    GT = auto()
    LE = auto()
    GE = auto()
    EQ = auto()
    NE = auto()
    
    ASSIGN = auto()
    IDENTIFIER = auto()
    
    EOF = auto()

class Token:
    def __init__(self, type_: TokenType, value: Any = None):
        self.type = type_
        self.value = value
    
    def __repr__(self):
        return f"Token({self.type.name}, {self.value})"

class Lexer:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos] if self.text else None

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None

    def peek(self) -> Optional[str]:
        peek_pos = self.pos + 1
        if peek_pos < len(self.text):
            return self.text[peek_pos]
        return None

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def number(self):
        result = ''
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            result += self.current_char
            self.advance()
        return Token(TokenType.NUMBER, float(result))

    def _id(self):
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
            
        if result == 'and': return Token(TokenType.AND, 'and')
        if result == 'or': return Token(TokenType.OR, 'or')
        if result == 'not': return Token(TokenType.NOT, 'not')
        if result == 'true': return Token(TokenType.TRUE, True)
        if result == 'false': return Token(TokenType.FALSE, False)
        
        return Token(TokenType.IDENTIFIER, result)

    def get_next_token(self) -> Token:
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            
            if self.current_char.isdigit():
                return self.number()
                
            if self.current_char.isalpha():
                return self._id()
            
            if self.current_char == '+':
                self.advance()
                return Token(TokenType.PLUS, '+')
            
            if self.current_char == '-':
                self.advance()
                return Token(TokenType.MINUS, '-')
            
            if self.current_char == '*':
                self.advance()
                return Token(TokenType.MUL, '*')
            
            if self.current_char == '/':
                self.advance()
                return Token(TokenType.DIV, '/')
            
            if self.current_char == '%':
                self.advance()
                return Token(TokenType.MOD, '%')
            
            if self.current_char == '(':
                self.advance()
                return Token(TokenType.LPAREN, '(')
            
            if self.current_char == ')':
                self.advance()
                return Token(TokenType.RPAREN, ')')
            
            if self.current_char == '=':
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.EQ, '==')
                self.advance()
                return Token(TokenType.ASSIGN, '=')
            
            if self.current_char == '!':
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.NE, '!=')
                raise Exception("Expected !=, got !")
            
            if self.current_char == '<':
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.LE, '<=')
                self.advance()
                return Token(TokenType.LT, '<')
            
            if self.current_char == '>':
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.GE, '>=')
                self.advance()
                return Token(TokenType.GT, '>')
            
            raise Exception(f"Invalid character: {self.current_char}")

        return Token(TokenType.EOF, None)

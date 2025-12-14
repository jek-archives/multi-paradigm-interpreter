from enum import Enum, auto
from typing import Any, Optional
from errors import LexerError

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
    def __init__(self, type_: TokenType, value: Any = None, lineno: int = None, column: int = None):
        self.type = type_
        self.value = value
        self.lineno = lineno
        self.column = column
    
    def __repr__(self):
        return f"Token({self.type.name}, {self.value}, Line:{self.lineno}, Col:{self.column})"

class Lexer:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.lineno = 1
        self.column = 1
        self.current_char = self.text[self.pos] if self.text else None

    def advance(self):
        if self.current_char == '\n':
            self.lineno += 1
            self.column = 0
        
        self.pos += 1
        self.column += 1
        
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
        lineno = self.lineno
        column = self.column
        result = ''
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            result += self.current_char
            self.advance()
        return Token(TokenType.NUMBER, float(result), lineno, column)

    def _id(self):
        lineno = self.lineno
        column = self.column
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
            
        if result == 'and': return Token(TokenType.AND, 'and', lineno, column)
        if result == 'or': return Token(TokenType.OR, 'or', lineno, column)
        if result == 'not': return Token(TokenType.NOT, 'not', lineno, column)
        if result == 'true': return Token(TokenType.TRUE, True, lineno, column)
        if result == 'false': return Token(TokenType.FALSE, False, lineno, column)
        
        return Token(TokenType.IDENTIFIER, result, lineno, column)

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
                token = Token(TokenType.PLUS, '+', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '-':
                token = Token(TokenType.MINUS, '-', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '*':
                token = Token(TokenType.MUL, '*', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '/':
                token = Token(TokenType.DIV, '/', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '%':
                token = Token(TokenType.MOD, '%', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '(':
                token = Token(TokenType.LPAREN, '(', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == ')':
                token = Token(TokenType.RPAREN, ')', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '=':
                start_col = self.column
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.EQ, '==', self.lineno, start_col)
                token = Token(TokenType.ASSIGN, '=', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '!':
                start_col = self.column
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.NE, '!=', self.lineno, start_col)
                raise LexerError("Expected !=, got !", self.lineno, self.column)
            
            if self.current_char == '<':
                start_col = self.column
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.LE, '<=', self.lineno, start_col)
                token = Token(TokenType.LT, '<', self.lineno, self.column)
                self.advance()
                return token
            
            if self.current_char == '>':
                start_col = self.column
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token(TokenType.GE, '>=', self.lineno, start_col)
                token = Token(TokenType.GT, '>', self.lineno, self.column)
                self.advance()
                return token
            
            raise LexerError(f"Invalid character: {self.current_char}", self.lineno, self.column)

        return Token(TokenType.EOF, None, self.lineno, self.column)

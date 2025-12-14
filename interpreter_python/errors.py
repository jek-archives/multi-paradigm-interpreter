class Error(Exception):
    def __init__(self, message, lineno=None, column=None):
        self.message = message
        self.lineno = lineno
        self.column = column
        super().__init__(message)

class LexerError(Error):
    pass

class ParserError(Error):
    pass

class InterpreterError(Error):
    pass

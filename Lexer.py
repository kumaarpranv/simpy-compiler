from pydantic import BaseModel
from Token import LookupIdent, Token, KEYCONSTS


class Lexer(BaseModel):
    input: str
    position: int
    readPosition: int
    ch: str

    def New(input):
        l = Lexer.parse_obj(
            {"input": input, "position": 0, "readPosition": 0, "ch": ""}
        )
        l.readChar()
        return l

    def readChar(__self):
        if __self.readPosition >= len(__self.input):
            __self.ch = 0
        else:
            __self.ch = __self.input[__self.readPosition]
        __self.position = __self.readPosition
        __self.readPosition += 1

    def NextToken(__self):
        tok = None
        __self.skipWhitespace()
        if __self.ch == "=":
            if __self.peekChar() == "=":
                ch = __self.ch
                __self.readChar()
                tok = Token.NewToken(KEYCONSTS.EQ, str(ch) + str(__self.ch))
            else:
                tok = Token.NewToken(KEYCONSTS.ASSIGN, __self.ch)
        elif __self.ch == ";":
            tok = Token.NewToken(KEYCONSTS.SEMICOLON, __self.ch)
        elif __self.ch == "(":
            tok = Token.NewToken(KEYCONSTS.LPAREN, __self.ch)
        elif __self.ch == ")":
            tok = Token.NewToken(KEYCONSTS.RPAREN, __self.ch)
        elif __self.ch == ",":
            tok = Token.NewToken(KEYCONSTS.COMMA, __self.ch)
        elif __self.ch == "+":
            tok = Token.NewToken(KEYCONSTS.PLUS, __self.ch)
        elif __self.ch == "-":
            tok = Token.NewToken(KEYCONSTS.MINUS, __self.ch)
        elif __self.ch == "!":
            if __self.peekChar() == "=":
                ch = __self.ch
                __self.readChar()
                tok = Token.NewToken(KEYCONSTS.NOT_EQ, str(ch) + str(__self.ch))
            else:
                tok = Token.NewToken(KEYCONSTS.BANG, __self.ch)
        elif __self.ch == "/":
            tok = Token.NewToken(KEYCONSTS.SLASH, __self.ch)
        elif __self.ch == "*":
            tok = Token.NewToken(KEYCONSTS.ASTERIK, __self.ch)
        elif __self.ch == "<":
            tok = Token.NewToken(KEYCONSTS.LT, __self.ch)
        elif __self.ch == ">":
            tok = Token.NewToken(KEYCONSTS.GT, __self.ch)
        elif __self.ch == "{":
            tok = Token.NewToken(KEYCONSTS.LBRACE, __self.ch)
        elif __self.ch == "}":
            tok = Token.NewToken(KEYCONSTS.RBRACE, __self.ch)
        elif __self.ch == 0:
            tok = Token.NewToken(KEYCONSTS.EOF, "")
        else:
            if isLetter(__self.ch):
                Literal = __self.readIdentifier()
                Type = LookupIdent(Literal)
                tok = Token.NewToken(Type, Literal)
                return tok
            elif isDigit(__self.ch):
                Type = KEYCONSTS.INT
                Literal = __self.readNumber()
                tok = Token.NewToken(Type, Literal)
                return tok

            else:
                tok = Token.NewToken(KEYCONSTS.ILLEGAL, __self.ch)

        __self.readChar()
        return tok

    def readIdentifier(__self):
        position = __self.position
        while isLetter(__self.ch):
            __self.readChar()

        return __self.input[position : __self.position]

    def skipWhitespace(__self):
        while (
            __self.ch == " "
            or __self.ch == "\t"
            or __self.ch == "\n"
            or __self.ch == "\r"
        ):
            __self.readChar()

    def peekChar(__self):
        if __self.readPosition >= len(__self.input):
            return 0
        else:
            return __self.input[__self.readPosition]

    def readNumber(__self):
        position = __self.position
        while isDigit(__self.ch):
            __self.readChar()
        return __self.input[position : __self.position]


def isLetter(ch):
    if not isinstance(ch, str):
        return False
    return ("a" <= ch and ch <= "z") or ("A" <= ch and ch <= "Z") or ch == "_"


def isDigit(ch):
    if not isinstance(ch, str):
        return False
    return "0" <= ch and ch <= "9"

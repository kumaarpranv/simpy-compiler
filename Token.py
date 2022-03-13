from pydantic import BaseModel
import enum


class Token(BaseModel):
    Type: enum.Enum
    Literal: str

    def NewToken(Type, Literal):
        t = Token.parse_obj({"Type": Type, "Literal": Literal})
        return t


class KEYCONSTS(enum.Enum):
    ILLEGAL = "ILLEGAL"
    EOF = "EOF"

    IDENT = "IDENT"
    INT = "INT"

    ASSIGN = "="
    PLUS = "+"
    MINUS = "-"
    BANG = "!"
    SLASH = "/"
    ASTERIK = "*"
    LT = "<"
    GT = ">"

    EQ = "=="
    NOT_EQ = "!="

    COMMA = ","
    SEMICOLON = ";"

    LPAREN = "("
    RPAREN = ")"
    LBRACE = "{"
    RBRACE = "}"

    FUNCTION = "FUNCTION"
    VAR = "VAR"
    TRUE = "TRUE"
    FALSE = "FALSE"
    IF = "IF"
    ELSE = "ELSE"
    RETURN = "RETURN"


Keywords = {
    "fn": KEYCONSTS.FUNCTION,
    "var": KEYCONSTS.VAR,
    "true": KEYCONSTS.TRUE,
    "false": KEYCONSTS.FALSE,
    "if": KEYCONSTS.IF,
    "else": KEYCONSTS.ELSE,
    "return": KEYCONSTS.RETURN,
}


def LookupIdent(ident):
    if ident in Keywords.keys():
        return Keywords[ident]
    else:
        return KEYCONSTS.IDENT

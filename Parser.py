from Token import *
from Lexer import *
from pydantic import BaseModel
from typing import Optional, List, Dict
from Ast import (
    BlockStatement,
    BooleanLiteral,
    CallExpression,
    Expression,
    ExpressionStatement,
    FunctionLiteral,
    IfExpression,
    InfixExpression,
    PrefixExpression,
    Program,
    VarStatement,
    Identifier,
    ReturnStatement,
    IntegerLiteral,
)
import enum
from structures.EnumComp import EnumComp


class PARSECONSTS(EnumComp):
    LOWEST = 1
    EQUALS = 2
    LESSGREATER = 3
    SUM = 4
    PRODUCT = 5
    PREFIX = 6
    CALL = 7


precedences = {
    KEYCONSTS.EQ: PARSECONSTS.EQUALS,
    KEYCONSTS.NOT_EQ: PARSECONSTS.EQUALS,
    KEYCONSTS.LT: PARSECONSTS.LESSGREATER,
    KEYCONSTS.GT: PARSECONSTS.LESSGREATER,
    KEYCONSTS.PLUS: PARSECONSTS.SUM,
    KEYCONSTS.MINUS: PARSECONSTS.SUM,
    KEYCONSTS.SLASH: PARSECONSTS.PRODUCT,
    KEYCONSTS.ASTERIK: PARSECONSTS.PRODUCT,
    KEYCONSTS.LPAREN: PARSECONSTS.CALL,
}


class Parser(BaseModel):
    l: Lexer
    curToken: Optional[Token]
    peekToken: Optional[Token]
    errors: List[str]
    prefixParseFns: Optional[Dict]
    infixParseFns: Optional[Dict]

    def New(l):
        p = Parser.parse_obj(
            {
                "l": l,
                "errors": [],
            }
        )
        p.prefixParseFns = {}
        p.registerPrefix(KEYCONSTS.IDENT, p.parseIdentifier)
        p.registerPrefix(KEYCONSTS.INT, p.parseIntegerLiteral)
        p.registerPrefix(KEYCONSTS.BANG, p.parsePrefixExpression)
        p.registerPrefix(KEYCONSTS.MINUS, p.parsePrefixExpression)
        p.registerPrefix(KEYCONSTS.TRUE, p.parseBooleanLiteral)
        p.registerPrefix(KEYCONSTS.FALSE, p.parseBooleanLiteral)
        p.registerPrefix(KEYCONSTS.LPAREN, p.parseGroupedExpression)
        p.registerPrefix(KEYCONSTS.IF, p.parseIfExpression)
        p.registerPrefix(KEYCONSTS.FUNCTION, p.parseFunctionExpression)

        p.infixParseFns = {}
        p.registerInfix(KEYCONSTS.PLUS, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.MINUS, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.SLASH, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.ASTERIK, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.EQ, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.NOT_EQ, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.LT, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.GT, p.parseInfixExpression)
        p.registerInfix(KEYCONSTS.LPAREN, p.parseCallExpression)

        p.NextToken()
        p.NextToken()
        return p

    def parseCallExpression(__self, function):
        exp = CallExpression.parse_obj({"Token": __self.curToken})
        exp.Function = function
        exp.Arguments = __self.parseCallArguments()
        return exp

    def parseCallArguments(__self):
        args = []
        if __self.peekTokenIs(KEYCONSTS.RPAREN):
            __self.NextToken()
            return args

        __self.NextToken()
        args.append(__self.parseExpression(PARSECONSTS.LOWEST))
        while __self.peekTokenIs(KEYCONSTS.COMMA):
            __self.NextToken()
            __self.NextToken()
            args.append(__self.parseExpression(PARSECONSTS.LOWEST))

        if not __self.expectPeek(KEYCONSTS.RPAREN):
            return None
        return args

    def parseFunctionExpression(__self):
        lit = FunctionLiteral.parse_obj({"Token": __self.curToken})
        if not __self.expectPeek(KEYCONSTS.LPAREN):
            return None
        lit.Parameters = __self.parseFunctionParameters()
        if not __self.expectPeek(KEYCONSTS.LBRACE):
            return None

        lit.Body = __self.parseBlockStatement()
        return lit

    def parseFunctionParameters(__self):
        identifiers = []
        if __self.peekTokenIs(KEYCONSTS.RPAREN):
            __self.NextToken()
            return identifiers
        __self.NextToken()
        ident = Identifier.parse_obj(
            {"Token": __self.curToken, "Value": __self.curToken.Literal}
        )
        identifiers.append(ident)

        while __self.peekTokenIs(KEYCONSTS.COMMA):
            __self.NextToken()
            __self.NextToken()
            ident = Identifier.parse_obj(
                {"Token": __self.curToken, "Value": __self.curToken.Literal}
            )
            identifiers.append(ident)

        if not __self.expectPeek(KEYCONSTS.RPAREN):
            return None
        return identifiers

    def parseIfExpression(__self):
        expression = IfExpression.parse_obj({"Token": __self.curToken})

        if not __self.expectPeek(KEYCONSTS.LPAREN):
            return None

        __self.NextToken()
        expression.Condition = __self.parseExpression(PARSECONSTS.LOWEST)
        if not __self.expectPeek(KEYCONSTS.RPAREN):
            return None

        if not __self.expectPeek(KEYCONSTS.LBRACE):
            return None

        expression.Consequence = __self.parseBlockStatement()

        if __self.peekTokenIs(KEYCONSTS.ELSE):
            __self.NextToken()

            if not __self.expectPeek(KEYCONSTS.LBRACE):
                return None
            expression.Alternative = __self.parseBlockStatement()

        return expression

    def parseBlockStatement(__self):
        block = BlockStatement.parse_obj({"Token": __self.curToken})
        block.Statements = []

        __self.NextToken()

        while (not __self.curTokenIs(KEYCONSTS.RBRACE)) and (
            not __self.curTokenIs(KEYCONSTS.EOF)
        ):
            stmt = __self.ParseStatement()
            if stmt != None:
                block.Statements.append(stmt)
            __self.NextToken()

        return block

    def parseGroupedExpression(__self):
        __self.NextToken()
        exp = __self.parseExpression(PARSECONSTS.LOWEST)

        if not __self.expectPeek(KEYCONSTS.RPAREN):
            return None
        return exp

    def parseInfixExpression(__self, left):

        expression = InfixExpression.parse_obj(
            {
                "Token": __self.curToken,
                "Operator": __self.curToken.Literal,
            }
        )
        expression.Left = left
        precedence = __self.curPrecedence()
        __self.NextToken()
        expression.Right = __self.parseExpression(precedence)

        return expression

    def parsePrefixExpression(__self):
        expression = PrefixExpression.parse_obj(
            {"Token": __self.curToken, "Operator": __self.curToken.Literal}
        )
        __self.NextToken()
        expression.Right = __self.parseExpression(PARSECONSTS.PREFIX)
        return expression

    def parseIdentifier(__self):
        return Identifier.parse_obj(
            {"Token": __self.curToken, "Value": __self.curToken.Literal}
        )

    def Errors(__self):
        return __self.errors

    def NextToken(__self):
        __self.curToken = __self.peekToken
        __self.peekToken = Lexer.NextToken(__self.l)

    def peekError(__self, t):
        msg = (
            "Expected next token to be "
            + t.value
            + ", got "
            + __self.peekToken.Type.value
        )
        __self.errors.append(msg)

    def parseBooleanLiteral(__self):
        return BooleanLiteral.parse_obj(
            {"Token": __self.curToken, "Value": __self.curTokenIs(KEYCONSTS.TRUE)}
        )

    def ParseProgram(__self):
        program = Program.parse_obj({"Statements": []})

        while __self.curToken.Type != KEYCONSTS.EOF:
            stmt = __self.ParseStatement()
            if stmt != None:
                program.Statements.append(stmt)
            __self.NextToken()

        return program

    def ParseStatement(__self):
        if __self.curToken.Type == KEYCONSTS.VAR:
            return __self.ParseVarStatement()
        elif __self.curToken.Type == KEYCONSTS.RETURN:
            return __self.parseReturnStatement()
        else:
            return __self.parseExpressionStatement()

    def parseIntegerLiteral(__self):
        lit = IntegerLiteral.parse_obj({"Token": __self.curToken})
        value = None
        if __self.curToken.Literal.isdigit():
            value = int(__self.curToken.Literal)
        else:
            __self.errors.append(
                "could not parse " + __self.curToken.Literal + " as integer"
            )
            return None

        lit.Value = value
        return lit

    def ParseVarStatement(__self):
        stmt = VarStatement.parse_obj({"Token": __self.curToken})

        if not __self.expectPeek(KEYCONSTS.IDENT):
            return None

        identifier = Identifier.parse_obj(
            {"Token": __self.curToken, "Value": __self.curToken.Literal}
        )
        stmt.Name = identifier

        if not __self.expectPeek(KEYCONSTS.ASSIGN):
            return None
        __self.NextToken()
        stmt.Value = __self.parseExpression(PARSECONSTS.LOWEST)
        if __self.peekTokenIs(KEYCONSTS.SEMICOLON):
            __self.NextToken()

        return stmt

    def parseReturnStatement(__self):
        stmt = ReturnStatement.parse_obj({"Token": __self.curToken})
        __self.NextToken()

        stmt.ReturnValue = __self.parseExpression(PARSECONSTS.LOWEST)

        if __self.peekTokenIs(KEYCONSTS.SEMICOLON):
            __self.NextToken()

        return stmt

    def noPrefixParseFnError(__self, t):
        msg = "no prefix parse function for " + t.value + " found"
        __self.errors.append(msg)

    def parseExpression(__self, precedence):
        prefix = __self.prefixParseFns[__self.curToken.Type]
        if prefix == None:
            __self.noPrefixParseFnError(__self.curToken.Type)
            return None
        leftExp = prefix()
        while (not __self.peekTokenIs(KEYCONSTS.SEMICOLON)) and (
            precedence < __self.peekPrecedence()
        ):
            infix = __self.infixParseFns[__self.peekToken.Type]
            if infix == None:
                return leftExp
            __self.NextToken()
            leftExp = infix(leftExp)
        return leftExp

    def parseExpressionStatement(__self):
        expr = __self.parseExpression(PARSECONSTS.LOWEST)
        stmt = ExpressionStatement.parse_obj({"Token": __self.curToken})
        stmt.Expression = expr
        if Parser.peekTokenIs(__self, KEYCONSTS.SEMICOLON):
            Parser.NextToken(__self)
        return stmt

    def curTokenIs(__self, t):
        return __self.curToken.Type == t

    def peekTokenIs(__self, t):
        return __self.peekToken.Type == t

    def expectPeek(__self, t):
        if __self.peekTokenIs(t):
            __self.NextToken()
            return True
        else:
            __self.peekError(t)
            return False

    def peekPrecedence(__self):
        if __self.peekToken.Type in precedences.keys():
            return precedences[__self.peekToken.Type]
        else:
            return PARSECONSTS.LOWEST

    def curPrecedence(__self):
        if __self.curToken.Type in precedences.keys():
            return precedences[__self.curToken.Type]
        else:
            return PARSECONSTS.LOWEST

    def registerPrefix(__self, tokenType, fn):
        __self.prefixParseFns[tokenType] = fn

    def registerInfix(__self, tokenType, fn):
        __self.infixParseFns[tokenType] = fn

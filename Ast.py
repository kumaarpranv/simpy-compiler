from pydantic import BaseModel
from Token import Token
from typing import List
from typing import Optional


class Node(BaseModel):
    def TokenLiteral(__self):
        pass


class Expression(BaseModel):
    Node: Node

    def expressionNode(__self):
        pass


class IntegerLiteral(BaseModel):
    Token: Token
    Value: Optional[int]

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class ExpressionStatement(BaseModel):
    Token: Token
    Expression: Optional[Expression]

    def statementNode(__self):
        return

    def TokenLiteral(__self):
        return str(__self.Token.Literal)


class Statement(BaseModel):
    Node: Node

    def statementNode(__self):
        pass


class Program(BaseModel):
    Statements: List[Statement]

    def TokenLiteral(__self):
        if len(__self.Statements) > 0:
            return __self.TokenLiteral(__self.Statements[0])
        else:
            return ""


class Identifier(BaseModel):
    Token: Token
    Value: str

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class BlockStatement(BaseModel):
    Token: Token
    Statements: Optional[List[Statement]]

    def statementNode(__self):
        pass

    def TokenLiteral(__self):
        return str(__self.Token.Literal)


class FunctionLiteral(BaseModel):
    Token: Token
    Parameters: Optional[List[Identifier]]
    Body: Optional[BlockStatement]

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class BooleanLiteral(BaseModel):
    Token: Token
    Value: bool

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class VarStatement(BaseModel):
    Token: Token
    Name: Optional[Identifier]
    Value: Optional[Expression]

    def statementNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class ReturnStatement(BaseModel):
    Token: Token
    ReturnValue: Optional[Expression]

    def statementNode(__self):
        pass

    def TokenLiteral(__self):
        return str(__self.Token.Literal)


class CallExpression(BaseModel):
    Token: Token
    Function: Optional[Expression]
    Arguments: Optional[List[Expression]]

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class IfExpression(BaseModel):
    Token: Token
    Condition: Optional[Expression]
    Consequence: Optional[BlockStatement]
    Alternative: Optional[BlockStatement]

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class PrefixExpression(BaseModel):
    Token: Token
    Operator: str
    Right: Optional[Expression]

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal


class InfixExpression(BaseModel):
    Token: Token
    Operator: str
    Left: Optional[Expression]
    Right: Optional[Expression]

    def expressionNode(__self):
        return

    def TokenLiteral(__self):
        return __self.Token.Literal

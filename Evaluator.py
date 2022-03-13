from Object import *
from Ast import *
import enum


class EVALCONSTS(enum.Enum):
    NULL = Null.parse_obj({"Value": None})
    TRUE = Boolean.parse_obj({"Value": True})
    FALSE = Boolean.parse_obj({"Value": False})


def evalBangOperatorExpression(right):
    if isinstance(right, Boolean):
        if right.Value == EVALCONSTS.TRUE.value.Value:
            return EVALCONSTS.FALSE.value
        elif right.Value == EVALCONSTS.FALSE.value.Value:
            return EVALCONSTS.TRUE.value

    elif isinstance(right, Null):
        return EVALCONSTS.TRUE.value
    else:
        return EVALCONSTS.FALSE.value


def evalMinusPrefixOperatorExpression(right):
    if right.Type() != OBJCONSTS.INTEGER_OBJ:
        return newError("unknown operator: -" + right.Type().value)
    value = right.Value
    return Integer.parse_obj({"Value": -value})


def evalPrefixExpression(operator, right):
    if operator == "!":
        return evalBangOperatorExpression(right)
    if operator == "-":
        return evalMinusPrefixOperatorExpression(right)
    return newError("unknown operator " + operator + "" + right.Type().value)


def evalIntegerInfixExpression(operator, left, right):

    leftVal = left.Value
    rightVal = right.Value
    if operator == "+":
        return Integer.parse_obj({"Value": leftVal + rightVal})
    elif operator == "-":
        return Integer.parse_obj({"Value": leftVal - rightVal})
    elif operator == "*":
        return Integer.parse_obj({"Value": leftVal * rightVal})
    elif operator == "/":
        return Integer.parse_obj({"Value": leftVal / rightVal})
    elif operator == "<":
        return nativeBoolToBooleanObject(leftVal < rightVal)
    elif operator == ">":
        return nativeBoolToBooleanObject(leftVal > rightVal)
    elif operator == "==":
        return nativeBoolToBooleanObject(leftVal == rightVal)
    elif operator == "!=":
        return nativeBoolToBooleanObject(leftVal != rightVal)
    else:
        return newError(
            "unknown operator: "
            + left.Type().value
            + " "
            + operator
            + " "
            + right.Type().value
        )


def isTrue(obj):
    if obj == EVALCONSTS.NULL.value:
        return False
    elif obj == EVALCONSTS.TRUE.value:
        return True
    elif obj == EVALCONSTS.FALSE.value:
        return False
    else:
        return True


def evalIfExpression(ie):
    condition = Eval(ie.Condition)
    if isError(condition):
        return condition
    if isTrue(condition):
        return Eval(ie.Consequence)
    elif ie.Alternative != None:
        return Eval(ie.Alternative)
    else:
        return None


def newError(msg):
    return Error.parse_obj({"Message": msg})


def evalInfixExpression(operator, left, right):
    if left.Type() == OBJCONSTS.INTEGER_OBJ and right.Type() == OBJCONSTS.INTEGER_OBJ:
        return evalIntegerInfixExpression(operator, left, right)

    if left.Type() != right.Type():
        return newError(
            "type mismatch: "
            + left.Type().value
            + " "
            + operator
            + " "
            + right.Type().value
        )

    if operator == "==":
        return nativeBoolToBooleanObject(left == right)
    if operator == "!=":
        return nativeBoolToBooleanObject(left != right)

    return newError(
        "unknown operator: "
        + left.Type().value
        + " "
        + operator
        + " "
        + right.Type().value
    )


def nativeBoolToBooleanObject(input):
    if input:
        return Boolean.parse_obj({"Value": True})
    else:
        return Boolean.parse_obj({"Value": False})


def evalStatements(stmts):
    result = None
    for stmt in stmts:
        result = Eval(stmt)
        if isinstance(result, ReturnValue):
            return result.Value
    return result


def evalProgram(program):
    result = None
    for stmt in program.Statements:
        result = Eval(stmt)
        if isinstance(result, ReturnValue):
            return result.Value
        if isinstance(result, Error):
            return result
    return result


def evalBlockStatement(block):
    result = None
    for stmt in block.Statements:
        result = Eval(stmt)
        if result != None:
            rt = result.Type()
            if rt == OBJCONSTS.RETURN_VALUE_OBJ or rt == OBJCONSTS.ERROR_OBJ:
                return result
    return result


def isError(obj):
    if obj != None:
        return obj.Type() == OBJCONSTS.ERROR_OBJ
    return False


def Eval(node):
    # print(type(node))
    if isinstance(node, Program):
        return evalProgram(node)
    elif isinstance(node, VarStatement):
        val = Eval(node.Value)
        if isError(val):
            return val
    elif isinstance(node, ExpressionStatement):
        return Eval(node.Expression)
    elif isinstance(node, IntegerLiteral):
        return Integer.parse_obj({"Value": node.Value})
    elif isinstance(node, BooleanLiteral):
        return nativeBoolToBooleanObject(node.Value)
    elif isinstance(node, PrefixExpression):
        right = Eval(node.Right)
        if isError(right):
            return right
        return evalPrefixExpression(node.Operator, right)

    elif isinstance(node, InfixExpression):
        left = Eval(node.Left)
        if isError(left):
            return left
        right = Eval(node.Right)
        if isError(right):
            return right
        return evalInfixExpression(node.Operator, left, right)

    elif isinstance(node, BlockStatement):
        return evalBlockStatement(node)
    elif isinstance(node, IfExpression):
        return evalIfExpression(node)
    elif isinstance(node, ReturnStatement):
        val = Eval(node.ReturnValue)
        if isError(val):
            return val
        return ReturnValue.parse_obj({"Value": val})
    elif isinstance(node, Program):
        return evalProgram(node)

    return None

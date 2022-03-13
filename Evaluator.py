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


def evalIfExpression(ie, env):
    condition = Eval(ie.Condition, env)
    if isError(condition):
        return condition
    if isTrue(condition):
        return Eval(ie.Consequence, env)
    elif ie.Alternative != None:
        return Eval(ie.Alternative, env)
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


def evalStatements(stmts, env):
    result = None
    for stmt in stmts:
        result = Eval(stmt, env)
        if isinstance(result, ReturnValue):
            return result.Value
    return result


def evalProgram(program, env):
    result = None
    for stmt in program.Statements:
        result = Eval(stmt, env)
        if isinstance(result, ReturnValue):
            return result.Value
        if isinstance(result, Error):
            return result
    return result


def evalBlockStatement(block, env):
    result = None
    for stmt in block.Statements:
        result = Eval(stmt, env)
        if result != None:
            rt = result.Type()
            if rt == OBJCONSTS.RETURN_VALUE_OBJ or rt == OBJCONSTS.ERROR_OBJ:
                return result
    return result


def isError(obj):
    NoneType = type(None)
    if isinstance(obj, NoneType):
        return obj.Type() == OBJCONSTS.ERROR_OBJ
    return False


def evalIdentifier(node, env):
    val = env.Get(node.Value)
    NoneType = type(None)

    if isinstance(val, NoneType):
        return newError("identifier not found: " + node.Value)
    else:
        return val


def evalExpressions(exps, env):
    result = []
    for e in exps:
        evaluated = Eval(e, env)
        if isError(evaluated):
            return [evaluated]
        result.append(evaluated)
    return result


def unwrapReturnValue(obj):
    if isinstance(obj, ReturnValue):
        return obj.Value
    return obj


def extendFunctionEnv(fn, args):
    env = Environment()
    env.SetOuter(fn.Env)
    for paramIdx, param in enumerate(fn.Parameters):
        env.Set(param.Value, args[paramIdx])
    return env


def applyFunction(fn, args):
    function = fn
    NoneType = type(None)
    if isinstance(function, NoneType):
        return newError("not a function: " + fn.Type().value)
    extendedEnv = extendFunctionEnv(function, args)
    evaluated = Eval(function.Body, extendedEnv)
    return unwrapReturnValue(evaluated)


def Eval(node, env):
    # print(type(node))
    if isinstance(node, Program):
        return evalProgram(node, env)
    elif isinstance(node, VarStatement):
        val = Eval(node.Value, env)
        if isError(val):
            return val
        env.Set(node.Name.Value, val)
    elif isinstance(node, Identifier):
        return evalIdentifier(node, env)

    elif isinstance(node, ExpressionStatement):
        return Eval(node.Expression, env)
    elif isinstance(node, IntegerLiteral):
        return Integer.parse_obj({"Value": node.Value})
    elif isinstance(node, BooleanLiteral):
        return nativeBoolToBooleanObject(node.Value)
    elif isinstance(node, FunctionLiteral):
        params = node.Parameters
        body = node.Body
        return Function.parse_obj({"Parameters": params, "Env": env, "Body": body})
    elif isinstance(node, CallExpression):
        function = Eval(node.Function, env)
        if isError(function):
            return function
        args = evalExpressions(node.Arguments, env)
        if len(args) == 1 and isError(args[0]):
            return args[0]
        return applyFunction(function, args)

    elif isinstance(node, PrefixExpression):
        right = Eval(node.Right, env)
        if isError(right):
            return right
        return evalPrefixExpression(node.Operator, right)

    elif isinstance(node, InfixExpression):
        left = Eval(node.Left, env)
        if isError(left):
            return left
        right = Eval(node.Right, env)
        if isError(right):
            return right
        return evalInfixExpression(node.Operator, left, right)

    elif isinstance(node, BlockStatement):
        return evalBlockStatement(node, env)
    elif isinstance(node, IfExpression):
        return evalIfExpression(node, env)
    elif isinstance(node, ReturnStatement):
        val = Eval(node.ReturnValue, env)
        if isError(val):
            return val
        return ReturnValue.parse_obj({"Value": val})
    elif isinstance(node, Program):
        return evalProgram(node, env)

    return None

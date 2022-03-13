from pydantic import BaseModel
import enum


class OBJCONSTS(enum.Enum):
    INTEGER_OBJ = "INTEGER"
    BOOLEAN_OBJ = "BOOLEAN"
    NULL_OBJ = "NULL"
    RETURN_VALUE_OBJ = "RETURN_VALUE"
    ERROR_OBJ = "ERROR"


class Environment(BaseModel):
    store = {}

    def Get(__self, name):
        if name in __self.store.keys():
            return __self.store[name]
        return None

    def Set(__self, name, value):
        __self.store[name] = value
        return value


class Object(BaseModel):
    def Type(__self):
        return

    def Inspect(__self):
        return


class Integer(Object):
    Value: int

    def Inspect(__self):
        return __self.Value

    def Type(__self):
        return OBJCONSTS.INTEGER_OBJ


class Boolean(Object):
    Value: int

    def Inspect(__self):
        return __self.Value

    def Type(__self):
        return OBJCONSTS.BOOLEAN_OBJ


class ReturnValue(Object):
    Value: Object

    def Inspect(__self):
        return __self.Value.Inspect()

    def Type(__self):
        return OBJCONSTS.RETURN_VALUE_OBJ


class Error(BaseModel):
    Message: str

    def Inspect(__self):
        return "Error: " + __self.Message

    def Type(__self):
        return OBJCONSTS.ERROR_OBJ


class Null(Object):
    Value: None

    def Inspect(__self):
        return __self.Value

    def Type(__self):
        return OBJCONSTS.NULL_OBJ

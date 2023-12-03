import copy

from enum import Enum
from intbase import InterpreterBase


# Enumerated type for our different language data types
class Type(Enum):
    INT = 1
    BOOL = 2
    STRING = 3
    CLOSURE = 4
    NIL = 5
    # My addition on top of type_valuev3: object enum
    OBJECT = 6


class Closure:
    def __init__(self, func_ast, env):
        self.captured_env = copy.deepcopy(env)
        # Edit to Carey's v3: Objects and closures are captured by reference
        for e, e_cp in zip(env.environment, self.captured_env.environment):
            for vname in e:
                if (isinstance(e[vname].v, Object) or isinstance(e[vname].v, Closure)
                    or isinstance(e[vname], Object) or isinstance(e[vname], Closure)):
                    e_cp[vname] = e[vname]                                            
        self.func_ast = func_ast
        self.type = Type.CLOSURE


# Represents a value, which has a type and its value
class Value:
    def __init__(self, t, v=None):
        self.t = t
        self.v = v

    def value(self):
        return self.v

    def type(self):
        return self.t

    def set(self, other):
        self.t = other.t
        self.v = other.v

# My addition on top of type_valuev3: object type
class Object:
    def __init__(self):
        # Treat vars and methods identically as first-class for now: keep them all in self.fields
        self.fields = dict()
        self.proto = None

# My addition to Carey's v3: dummy support to allow setting proto field explicitly to nil
class NilProto:
    def __init__(self):
        pass

# Catch edge case of using pass-by-reference to set a proto to a non-obj
class ProtoValue(Value):
    def __init__(self, t, v=None):
        super().__init__(t, v)
    def cpValue(self, v):
        self.t = v.t
        self.v = v.v
        

def create_value(val):
    if val == InterpreterBase.TRUE_DEF:
        return Value(Type.BOOL, True)
    elif val == InterpreterBase.FALSE_DEF:
        return Value(Type.BOOL, False)
    elif isinstance(val, int):
        return Value(Type.INT, val)
    elif val == InterpreterBase.NIL_DEF:
        return Value(Type.NIL, None)
    elif isinstance(val, str):
        return Value(Type.STRING, val)


def get_printable(val):
    if val.type() == Type.INT:
        return str(val.value())
    if val.type() == Type.STRING:
        return val.value()
    if val.type() == Type.BOOL:
        if val.value() is True:
            return "true"
        return "false"
    return None

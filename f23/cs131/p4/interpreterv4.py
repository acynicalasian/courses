import copy

# Change from Carey's v3: use regex to find dot notation use
import re

from enum import Enum

from brewparse import parse_program
from env_v3 import EnvironmentManager
from intbase import InterpreterBase, ErrorType

# Change from Carey's v3: use type_valuev4 instead of ""v3 and import Object too
from type_valuev4 import Closure, Type, Value, create_value, get_printable, Object, NilProto, ProtoValue


class ExecStatus(Enum):
    CONTINUE = 1
    RETURN = 2


# Main interpreter class
class Interpreter(InterpreterBase):
    # constants
    NIL_VALUE = create_value(InterpreterBase.NIL_DEF)
    TRUE_VALUE = create_value(InterpreterBase.TRUE_DEF)
    BIN_OPS = {"+", "-", "*", "/", "==", "!=", ">", ">=", "<", "<=", "||", "&&"}

    # Addition to Carey's v3: regex to find dot notation use
    __RE_LH = r"(?P<lh>(?P<us1>_+)(?(us1)[a-zA-Z0-9]+|[a-zA-Z]+)\w*)\."
    __RE_RH = r"(?P<rh>(?P<us2>_+)(?(us2)[a-zA-Z0-9]+|[a-zA-Z]+)\w*)"
    #__DOTREGEX = re.compile(__RE_LH + __RE_RH)
    __DOTREGEX = re.compile(r"(?P<lh>((_+[a-zA-Z0-9]+\w*)|([a-zA-Z]+\w*)))\.(?P<rh>((_+[a-zA-Z0-9]+\w*)|([a-zA-Z]+\w*)))")

    # methods
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.trace_output = trace_output
        self.__setup_ops()

    # run a program that's provided in a string
    # usese the provided Parser found in brewparse.py to parse the program
    # into an abstract syntax tree (ast)
    def run(self, program):
        ast = parse_program(program)
        self.__set_up_function_table(ast)
        self.env = EnvironmentManager()
        main_func = self.__get_func_by_name("main", 0)
        if main_func is None:
            super().error(ErrorType.NAME_ERROR, f"Function main not found")
        self.__run_statements(main_func.func_ast.get("statements"))

    def __set_up_function_table(self, ast):
        self.func_name_to_ast = {}
        empty_env = EnvironmentManager()
        for func_def in ast.get("functions"):
            func_name = func_def.get("name")
            num_params = len(func_def.get("args"))
            if func_name not in self.func_name_to_ast:
                self.func_name_to_ast[func_name] = {}
            self.func_name_to_ast[func_name][num_params] = Closure(func_def, empty_env)

    def __get_func_by_name(self, name, num_params):
        # My notes:
        # If not in function table, grabs a Closure stored in a Value I think?
        if name not in self.func_name_to_ast:
            closure_val_obj = self.env.get(name)
            if closure_val_obj is None:
                return None
                # super().error(ErrorType.NAME_ERROR, f"Function {name} not found")
            if closure_val_obj.type() != Type.CLOSURE:
                super().error(
                    ErrorType.TYPE_ERROR, "Trying to call function with non-closure"
                )
            closure = closure_val_obj.value()
            num_formal_params = len(closure.func_ast.get("args"))
            if num_formal_params != num_params:
                super().error(ErrorType.TYPE_ERROR, "Invalid # of args to lambda")
            return closure_val_obj.value()

        candidate_funcs = self.func_name_to_ast[name]
        if num_params is None:
            # case where we want assign variable to func_name and we don't have
            # a way to specify the # of arguments for the function, so we generate
            # an error if there's more than one function with that name
            if len(candidate_funcs) > 1:
                super().error(
                    ErrorType.NAME_ERROR,
                    f"Function {name} has multiple overloaded versions",
                )
            num_args = next(iter(candidate_funcs))
            closure = candidate_funcs[num_args]
            return closure

        if num_params not in candidate_funcs:
            super().error(
                ErrorType.NAME_ERROR,
                f"Function {name} taking {num_params} params not found",
            )
        return candidate_funcs[num_params]

    def __run_statements(self, statements):
        self.env.push()
        for statement in statements:
            if self.trace_output:
                print(statement)
            status = ExecStatus.CONTINUE
            if statement.elem_type == InterpreterBase.FCALL_DEF:
                self.__call_func(statement)
            # Addition to Carey's v3: add support for method calls now
            elif statement.elem_type == "mcall":
                self.__call_method(statement)
            elif statement.elem_type == "=":
                self.__assign(statement)
            elif statement.elem_type == InterpreterBase.RETURN_DEF:
                status, return_val = self.__do_return(statement)
            elif statement.elem_type == Interpreter.IF_DEF:
                status, return_val = self.__do_if(statement)
            elif statement.elem_type == Interpreter.WHILE_DEF:
                status, return_val = self.__do_while(statement)

            if status == ExecStatus.RETURN:
                self.env.pop()
                return (status, return_val)

        self.env.pop()
        return (ExecStatus.CONTINUE, Interpreter.NIL_VALUE)


    def __call_func(self, call_ast):
        func_name = call_ast.get("name")
        if func_name == "print":
            return self.__call_print(call_ast)
        if func_name == "inputi":
            return self.__call_input(call_ast)

        actual_args = call_ast.get("args")
        target_closure = self.__get_func_by_name(func_name, len(actual_args))
        if target_closure == None:
            super().error(ErrorType.NAME_ERROR, f"Function {func_name} not found")
        if target_closure.type != Type.CLOSURE:
            super().error(ErrorType.TYPE_ERROR, f"Function {func_name} is changed to non-function type.")
        target_ast = target_closure.func_ast

        new_env = {}
        self.__prepare_env_with_closed_variables(target_closure, new_env)
        self.__prepare_params(target_ast,call_ast, new_env)
        self.env.push(new_env)
        _, return_val = self.__run_statements(target_ast.get("statements"))
        self.env.pop()
        return return_val

    # Addition to Carey's v3: add support for method calls, reuse code from __call_func; messy but ehhh
    def __call_method(self, call_ast):
        m_name = call_ast.get("name")
        actual_args = call_ast.get("args")
        lh_name = call_ast.get("objref")
        objref = self.env.get(lh_name)
        # print("DEBUGGING:")
        # print("-> lh_name = " + lh_name)
        # print("-> m_name = " + m_name)
        if objref is None:
            super().error(ErrorType.NAME_ERROR, "Variable " + lh_name + " not found")
        # I don't think we need to use self.__get_func_by_name bc we can't set obj fields to overloaded fns, thank god
        elif not isinstance(objref.v, Object):
            super().error(ErrorType.TYPE_ERROR, "Trying to call method on non-object")
        elif m_name not in objref.v.fields:
            if objref.v.proto is not None:
                protoptr = objref.v.proto
                bad = True
                while protoptr:
                    if m_name in protoptr.fields:
                        if not isinstance(protoptr.fields[m_name].v, Closure):
                            super().error(ErrorType.TYPE_ERROR,
                                          "Trying to call non-function/closure")
                        if len(protoptr.fields[m_name].v.func_ast.get("args")) != len(actual_args):
                            super().error(ErrorType.NAME_ERROR, "Method " + lh_name + "." + m_name +
                                          " taking " + str(len(protoptr.fields[m_name].func_ast.get("args"))) +
                                          " params not found")
                        target_closure = protoptr.fields[m_name].v
                        target_ast = target_closure.func_ast
                        bad = False
                        break
                    protoptr = protoptr.proto
                if bad:
                    super().error(ErrorType.NAME_ERROR, "Method " + lh_name + "." + m_name + " not found")
            else:
                super().error(ErrorType.NAME_ERROR, "Method " + lh_name + "." + m_name + " not found")
        elif not isinstance(objref.v.fields[m_name].v, Closure):
            # print("DEBUGGING: lh_name was " + lh_name)
            # print("DEBUGGING: objref.v.fields[m_name] was type: " + str(type(objref.v.fields[m_name])))
            super().error(ErrorType.TYPE_ERROR, "Trying to call non-function/closure")
        elif len(objref.v.fields[m_name].v.func_ast.get("args")) != len(actual_args):
            super().error(ErrorType.NAME_ERROR, "Method " + lh_name + "." + m_name + " taking " +
                          str(len(objref.fields[m_name].func_ast.get("args"))) + " params not found")
        else:
            target_closure = objref.v.fields[m_name].v
            target_ast = target_closure.func_ast

        new_env = {}
        self.__prepare_env_with_closed_variables(target_closure, new_env)
        self.__prepare_params(target_ast, call_ast, new_env)
        new_env["this"] = objref
        self.env.push(new_env)
        _, return_val = self.__run_statements(target_ast.get("statements"))
        self.env.pop()
        return return_val

    def __prepare_env_with_closed_variables(self, target_closure, temp_env):
        for var_name, value in target_closure.captured_env:
            # Updated here - ignore updates to the scope if we
            #   altered a parameter, or if the argument is a similarly named variable
            temp_env[var_name] = value


    def __prepare_params(self, target_ast, call_ast, temp_env):
        actual_args = call_ast.get("args")
        formal_args = target_ast.get("args")
        if len(actual_args) != len(formal_args):
            super().error(
                ErrorType.NAME_ERROR,
                f"Function {target_ast.get('name')} with {len(actual_args)} args not found",
            )

        for formal_ast, actual_ast in zip(formal_args, actual_args):
            if formal_ast.elem_type == InterpreterBase.REFARG_DEF:
                # Edit to Carey's v3: check for passing a proto as reference
                if actual_ast.elem_type == "var":
                    objcheck = Interpreter.__DOTREGEX.fullmatch(actual_ast.dict["name"])
                    if objcheck:
                        if objcheck.group("rh") == "proto":
                            print("DEBUGGING: caught proto being passed " + actual_ast.dict["name"])
                            pv_rh = self.__eval_expr(actual_ast)
                            result = ProtoValue(pv_rh.t, pv_rh.v)
                            assert result.t is self.__eval_expr(actual_ast).t
                            assert result.v is self.__eval_expr(actual_ast).v
                        else:
                            result = self.__eval_expr(actual_ast)
                    else:
                        result = self.__eval_expr(actual_ast)
                else:
                    result = self.__eval_expr(actual_ast)
            else:
                result = copy.deepcopy(self.__eval_expr(actual_ast))
            arg_name = formal_ast.get("name")
            temp_env[arg_name] = result

    def __call_print(self, call_ast):
        output = ""
        for arg in call_ast.get("args"):
            result = self.__eval_expr(arg)  # result is a Value object
            output = output + get_printable(result)
        super().output(output)
        return Interpreter.NIL_VALUE

    def __call_input(self, call_ast):
        args = call_ast.get("args")
        if args is not None and len(args) == 1:
            result = self.__eval_expr(args[0])
            super().output(get_printable(result))
        elif args is not None and len(args) > 1:
            super().error(
                ErrorType.NAME_ERROR, "No inputi() function that takes > 1 parameter"
            )
        inp = super().get_input()
        if call_ast.get("name") == "inputi":
            return Value(Type.INT, int(inp))
        if call_ast.get("name") == "inputs":
            return Value(Type.STRING, inp)

    def __assign(self, assign_ast):
        var_name = assign_ast.get("name")
        rh = self.__eval_expr(assign_ast.get("expression"))
        # New code replacing Carey's intv3 to support objs
        objcheck = Interpreter.__DOTREGEX.fullmatch(var_name)
        if objcheck:
            # Setting a field to a closure/object points directly
            if isinstance(rh.v, Closure) or isinstance(rh.v, Object):
                src_value_obj = rh
            else:
                src_value_obj = copy.copy(rh)
            objname = objcheck.group("lh")
            fieldname = objcheck.group("rh")
            checkObjExists = self.env.get(objname)
            if checkObjExists is None:
                super().error(ErrorType.NAME_ERROR, "Variable " + objname + " not found")
            elif checkObjExists.t != Type.OBJECT:
                super().error(ErrorType.TYPE_ERROR, "Dot operator used on non-object " + objname)
            else:
                if fieldname == "proto":
                    if src_value_obj.v is None:
                        checkObjExists.v.proto = NilProto()
                    elif isinstance(src_value_obj.v, Object):
                        checkObjExists.v.proto = src_value_obj.v
                    else:
                        super().error(ErrorType.TYPE_ERROR, "Can't set proto to non-object type")
                    return
                else:
                    checkObjExists.v.fields[fieldname] = src_value_obj
        else:
            # Setting a var to another var that refers to an obj, they corefer in memory
            if isinstance(rh.v, Object):
                src_value_obj = rh
            else:
                src_value_obj = copy.copy(rh)
            target_value_obj = self.env.get(var_name)
            # Catch edge case of passing a proto as pass-by-ref and trying to set it to a non-obj
            if isinstance(target_value_obj, ProtoValue):
                if not isinstance(src_value_obj.v, Object):
                    super().error(ErrorType.TYPE_ERROR, "Can't set proto to non-object type")
            if target_value_obj is None:
                self.env.set(var_name, src_value_obj)
            else:
                # if a close is changed to another type such as int, we cannot make function calls on it any more
                # ^^ this comment was written by Carey in v3, but I don't see how this becomes an issue if we're
                # replacing the Closure with a new Value/primitive anyway? Whatever, he prolly knows better
                if target_value_obj.t == Type.CLOSURE and src_value_obj.t != Type.CLOSURE:
                    target_value_obj.v.type = src_value_obj.t
                target_value_obj.set(src_value_obj)
                
        # Old from Carey's intv3: rewrite this to support dot notation and objs
        # var_name = assign_ast.get("name")
        # src_value_obj = copy.copy(self.__eval_expr(assign_ast.get("expression")))
        # target_value_obj = self.env.get(var_name)
        # if target_value_obj is None:
        #     self.env.set(var_name, src_value_obj)
        # else:
        #     # if a close is changed to another type such as int, we cannot make function calls on it any more 
        #     if target_value_obj.t == Type.CLOSURE and src_value_obj.t != Type.CLOSURE:
        #         target_value_obj.v.type = src_value_obj.t
        #     target_value_obj.set(src_value_obj)

    def __eval_expr(self, expr_ast):
        if expr_ast.elem_type == InterpreterBase.NIL_DEF:
            return Interpreter.NIL_VALUE
        if expr_ast.elem_type == InterpreterBase.INT_DEF:
            return Value(Type.INT, expr_ast.get("val"))
        if expr_ast.elem_type == InterpreterBase.STRING_DEF:
            return Value(Type.STRING, expr_ast.get("val"))
        if expr_ast.elem_type == InterpreterBase.BOOL_DEF:
            return Value(Type.BOOL, expr_ast.get("val"))
        if expr_ast.elem_type == InterpreterBase.VAR_DEF:
            return self.__eval_name(expr_ast)
        if expr_ast.elem_type == InterpreterBase.FCALL_DEF:
            return self.__call_func(expr_ast)
        if expr_ast.elem_type in Interpreter.BIN_OPS:
            return self.__eval_op(expr_ast)
        if expr_ast.elem_type == Interpreter.NEG_DEF:
            return self.__eval_unary(expr_ast, Type.INT, lambda x: -1 * x)
        if expr_ast.elem_type == Interpreter.NOT_DEF:
            return self.__eval_unary(expr_ast, Type.BOOL, lambda x: not x)
        if expr_ast.elem_type == Interpreter.LAMBDA_DEF:
            return Value(Type.CLOSURE, Closure(expr_ast, self.env))
        # My addition to Carey's intv3: support object instantiation via '@'
        if expr_ast.elem_type == '@':
            return Value(Type.OBJECT, Object())

    def __eval_name(self, name_ast):
        var_name = name_ast.get("name")

        # New code to support prototypical inheritance
        objcheck = Interpreter.__DOTREGEX.fullmatch(var_name)
        if objcheck:
            objname = objcheck.group("lh")
            fieldname = objcheck.group("rh")
            checkObjExists = self.env.get(objname)
            if checkObjExists is None:
                super().error(ErrorType.NAME_ERROR, "Variable " + objname + " not found")
            elif checkObjExists.t != Type.OBJECT:
                super().error(ErrorType.TYPE_ERROR, "Dot operator used on non-object " + objname)
            if fieldname in checkObjExists.v.fields:
                if isinstance(checkObjExists.v.fields[fieldname], Object):
                    return Value(Type.OBJECT, checkObjExists.v.fields[fieldname])
                else:
                    return checkObjExists.v.fields[fieldname]
            elif fieldname == "proto":
                if checkObjExists.v.proto is None:
                    super().error(ErrorType.NAME_ERROR, "Field proto not found in object " + objname)
                elif isinstance(checkObjExists.v.proto, NilProto):
                    return Value(Type.NIL, None)
                return Value(Type.OBJECT, checkObjExists.v.proto)
            elif checkObjExists.v.proto is not None and not isinstance(checkObjExists.v.proto, NilProto):
                protoptr = checkObjExists.v.proto
                while protoptr:
                    if fieldname in protoptr.fields:
                        if isinstance(protoptr.fields[fieldname], Object):
                            return Value(Type.OBJECT, protoptr.fields[fieldname])
                        else:
                            return protoptr.fields[fieldname]
                    protoptr = protoptr.proto
            super().error(ErrorType.NAME_ERROR, "Field " + fieldname + " not found in object " + objname)
        else:
            val = self.env.get(var_name)
            if val is not None:
                return val
            closure = self.__get_func_by_name(var_name, None)
            if closure is None:
                super().error(
                    ErrorType.NAME_ERROR, f"Variable/function {var_name} not found"
                )
            return Value(Type.CLOSURE, closure)
        

        # Old code from Carey's v3: need to update to support prototypical inheritance
        # val = self.env.get(var_name)
        # if val is not None:
        #     return val
        # closure = self.__get_func_by_name(var_name, None)
        # if closure is None:
        #     super().error(
        #         ErrorType.NAME_ERROR, f"Variable/function {var_name} not found"
        #     )
        # return Value(Type.CLOSURE, closure)

    def __eval_op(self, arith_ast):
        left_value_obj = self.__eval_expr(arith_ast.get("op1"))
        right_value_obj = self.__eval_expr(arith_ast.get("op2"))

        # Hacky, but we can "shortcut" the code here
        # Testing: additions to op_to_lambda should fix the need for this and be more "smart"
        # if isinstance(left_value_obj, Value) and isinstance(right_value_obj, Value):
        #     if isinstance(left_value_obj.v, Object) and isinstance(right_value_obj.v, Object):
        #         return Value(Type.BOOL, left_value_obj.v is right_value_obj.v)
        # elif isinstance(left_value_obj, Value) and isinstance(right_value_obj, Object):
        #     if isinstance(left_value_obj.v, Object):
        #         return Value(Type.BOOL, left_value_obj.v is right_value_obj)
        # elif isinstance(left_value_obj, Object) and isinstance(right_value_obj, Value):
        #     if isinstance(right_value_obj.v, Object):
        #         return Value(Type.BOOL, left_value_obj is right_value_obj.v)


        left_value_obj, right_value_obj = self.__bin_op_promotion(
            arith_ast.elem_type, left_value_obj, right_value_obj
        )

        if not self.__compatible_types(
            arith_ast.elem_type, left_value_obj, right_value_obj
        ):
            super().error(
                ErrorType.TYPE_ERROR,
                f"Incompatible types for {arith_ast.elem_type} operation",
            )
        if arith_ast.elem_type not in self.op_to_lambda[left_value_obj.type()]:
            super().error(
                ErrorType.TYPE_ERROR,
                f"Incompatible operator {arith_ast.elem_type} for type {left_value_obj.type()}",
            )
        f = self.op_to_lambda[left_value_obj.type()][arith_ast.elem_type]
        return f(left_value_obj, right_value_obj)

    # bool and int, int and bool for and/or/==/!= -> coerce int to bool
    # bool and int, int and bool for arithmetic ops, coerce true to 1, false to 0
    def __bin_op_promotion(self, operation, op1, op2):
        if operation in self.op_to_lambda[Type.BOOL]:  # && or ||
            
            # If this operation is still allowed in the ints, then continue
            if operation in self.op_to_lambda[Type.INT] and op1.type() == Type.INT \
                and op2.type() == Type.INT:
                pass
            else:
                if op1.type() == Type.INT:
                    op1 = Interpreter.__int_to_bool(op1)
                if op2.type() == Type.INT:
                    op2 = Interpreter.__int_to_bool(op2)
        if operation in self.op_to_lambda[Type.INT]:  # +, -, *, /
            if op1.type() == Type.BOOL:
                op1 = Interpreter.__bool_to_int(op1)
            if op2.type() == Type.BOOL:
                op2 = Interpreter.__bool_to_int(op2)
        return (op1, op2)

    def __unary_op_promotion(self, operation, op1):
        if operation == "!" and op1.type() == Type.INT:
            op1 = Interpreter.__int_to_bool(op1)
        return op1

    @staticmethod
    def __int_to_bool(value):
        return Value(Type.BOOL, value.value() != 0)

    @staticmethod
    def __bool_to_int(value):
        return Value(Type.INT, 1 if value.value() else 0)

    def __compatible_types(self, oper, obj1, obj2):
        # DOCUMENT: allow comparisons ==/!= of anything against anything
        if oper in ["==", "!="]:
            return True
        return obj1.type() == obj2.type()

    def __eval_unary(self, arith_ast, t, f):
        value_obj = self.__eval_expr(arith_ast.get("op1"))
        value_obj = self.__unary_op_promotion(arith_ast.elem_type, value_obj)

        if value_obj.type() != t:
            super().error(
                ErrorType.TYPE_ERROR,
                f"Incompatible type for {arith_ast.elem_type} operation",
            )
        return Value(t, f(value_obj.value()))

    def __setup_ops(self):
        self.op_to_lambda = {}
        # set up operations on integers
        self.op_to_lambda[Type.INT] = {}
        self.op_to_lambda[Type.INT]["+"] = lambda x, y: Value(
            x.type(), x.value() + y.value()
        )
        self.op_to_lambda[Type.INT]["-"] = lambda x, y: Value(
            x.type(), x.value() - y.value()
        )
        self.op_to_lambda[Type.INT]["*"] = lambda x, y: Value(
            x.type(), x.value() * y.value()
        )
        self.op_to_lambda[Type.INT]["/"] = lambda x, y: Value(
            x.type(), x.value() // y.value()
        )
        self.op_to_lambda[Type.INT]["=="] = lambda x, y: Value(
            Type.BOOL, x.value() == y.value()
        )
        self.op_to_lambda[Type.INT]["!="] = lambda x, y: Value(
            Type.BOOL, x.value() != y.value()
        )
        self.op_to_lambda[Type.INT]["<"] = lambda x, y: Value(
            Type.BOOL, x.value() < y.value()
        )
        self.op_to_lambda[Type.INT]["<="] = lambda x, y: Value(
            Type.BOOL, x.value() <= y.value()
        )
        self.op_to_lambda[Type.INT][">"] = lambda x, y: Value(
            Type.BOOL, x.value() > y.value()
        )
        self.op_to_lambda[Type.INT][">="] = lambda x, y: Value(
            Type.BOOL, x.value() >= y.value()
        )
        #  set up operations on strings
        self.op_to_lambda[Type.STRING] = {}
        self.op_to_lambda[Type.STRING]["+"] = lambda x, y: Value(
            x.type(), x.value() + y.value()
        )
        self.op_to_lambda[Type.STRING]["=="] = lambda x, y: Value(
            Type.BOOL, x.value() == y.value()
        )
        self.op_to_lambda[Type.STRING]["!="] = lambda x, y: Value(
            Type.BOOL, x.value() != y.value()
        )
        #  set up operations on bools
        self.op_to_lambda[Type.BOOL] = {}
        self.op_to_lambda[Type.BOOL]["&&"] = lambda x, y: Value(
            x.type(), x.value() and y.value()
        )
        self.op_to_lambda[Type.BOOL]["||"] = lambda x, y: Value(
            x.type(), x.value() or y.value()
        )
        self.op_to_lambda[Type.BOOL]["=="] = lambda x, y: Value(
            Type.BOOL, x.value() == y.value()
        )
        self.op_to_lambda[Type.BOOL]["!="] = lambda x, y: Value(
            Type.BOOL, x.value() != y.value()
        )

        #  set up operations on nil
        self.op_to_lambda[Type.NIL] = {}
        self.op_to_lambda[Type.NIL]["=="] = lambda x, y: Value(
            Type.BOOL, x.value() == y.value()
        )
        self.op_to_lambda[Type.NIL]["!="] = lambda x, y: Value(
            Type.BOOL, x.value() != y.value()
        )

        #  set up operations on closures
        self.op_to_lambda[Type.CLOSURE] = {}
        self.op_to_lambda[Type.CLOSURE]["=="] = lambda x, y: Value(
            Type.BOOL, x.value() == y.value()
        )
        self.op_to_lambda[Type.CLOSURE]["!="] = lambda x, y: Value(
            Type.BOOL, x.value() != y.value()
        )

        # My addition to Carey's v3: add support for ops
        self.op_to_lambda[Type.OBJECT] = {}
        self.op_to_lambda[Type.OBJECT]["=="] = lambda x, y: Value(
            Type.BOOL, x.v is y.v
        )
        self.op_to_lambda[Type.OBJECT]["!="] = lambda x, y: Value(
            Type.BOOL, x.v is not y.v
        )

    def __do_if(self, if_ast):
        cond_ast = if_ast.get("condition")
        result = self.__eval_expr(cond_ast)
        if result.type() == Type.INT:
            result = Interpreter.__int_to_bool(result)
        if result.type() != Type.BOOL:
            super().error(
                ErrorType.TYPE_ERROR,
                "Incompatible type for if condition",
            )
        if result.value():
            statements = if_ast.get("statements")
            status, return_val = self.__run_statements(statements)
            return (status, return_val)
        else:
            else_statements = if_ast.get("else_statements")
            if else_statements is not None:
                status, return_val = self.__run_statements(else_statements)
                return (status, return_val)

        return (ExecStatus.CONTINUE, Interpreter.NIL_VALUE)

    def __do_while(self, while_ast):
        cond_ast = while_ast.get("condition")
        run_while = Interpreter.TRUE_VALUE
        while run_while.value():
            run_while = self.__eval_expr(cond_ast)
            if run_while.type() == Type.INT:
                run_while = Interpreter.__int_to_bool(run_while)
            if run_while.type() != Type.BOOL:
                super().error(
                    ErrorType.TYPE_ERROR,
                    "Incompatible type for while condition",
                )
            if run_while.value():
                statements = while_ast.get("statements")
                status, return_val = self.__run_statements(statements)
                if status == ExecStatus.RETURN:
                    return status, return_val

        return (ExecStatus.CONTINUE, Interpreter.NIL_VALUE)

    def __do_return(self, return_ast):
        expr_ast = return_ast.get("expression")
        if expr_ast is None:
            return (ExecStatus.RETURN, Interpreter.NIL_VALUE)
        value_obj = copy.deepcopy(self.__eval_expr(expr_ast))
        return (ExecStatus.RETURN, value_obj)

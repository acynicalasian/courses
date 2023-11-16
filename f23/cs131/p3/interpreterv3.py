from brewparse import parse_program
import intbase
import copy

class IntRef:
    def __init__(self, n):
        self.val = n
            
class StrRef:
    def __init__(self, s):
        self.val = s

class BoolRef:
    def __init__(self, b):
        self.val = b

def isRefType(obj):
    if isinstance(obj, IntRef) or isinstance(obj, StrRef) or isinstance(obj, BoolRef):
        return True
    else:
        return False

class LambdaClosure:
    def __init__(self, l, local_vtable, local_ftable):
        self.fdef = l.dict["statements"]
        self.args = l.dict["args"]
        self.vcaptures = copy.deepcopy(local_vtable)
        self.fcaptures = copy.deepcopy(local_ftable)

class Interpreter(intbase.InterpreterBase):
    __binops = ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">=", "||", "&&"]
    __unops = ["neg", '!']
    __datatypes = ["int", "string", "bool"]

    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.__vtable = dict()    # vtable for main()
        self.__ftable = dict()    # ftable for main()

        # For returning from inside if and while blocks
        self.__nestedbreak = { "flag" : False, "value" : None }

    def run(self, program):
        parsed_program = parse_program(program)
        if len(parsed_program.dict["functions"]) < 1:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No main() function was found")

        # Store a function table and a pointer to the main function
        found_main = False
        main = None
        for f in parsed_program.dict["functions"]:
            fname = f.dict["name"]
            if fname == "main":
                found_main = True
                main = f
            else:
                try:
                    keyexists = self.__ftable[fname]
                except KeyError:
                    self.__ftable[fname] = []
                self.__ftable[fname].append(f)
        if found_main == False:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No main() function was found")

        for s in main.dict["statements"]:
            if s.elem_type == '=':
                self.eval_assign(s, self.__vtable, self.__ftable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, self.__vtable, self.__ftable)
            elif s._elem_type == "if":
                self.eval_if(s, self.__vtable, self.__ftable)
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["value"]
            elif s.elem_type == "while":
                self.eval_while(s, self.__vtable, self.__ftable)
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["value"]
            elif s.elem_type == "return":
                return self.eval_return(s, self.__vtable, self.__ftable)
        return 0

    def eval_assign(self, s, local_vtable, local_ftable):
        lh = s.dict["name"]
        rh_node = s.dict["expression"]
        rh = None
        if rh_node.elem_type in Interpreter.__binops:
            rh = self.eval_binop(rh_node, local_vtable, local_ftable)
        elif rh_node.elem_type in Interpreter.__unops:
            rh = self.eval_unary(rh_node, local_vtable, local_ftable)
        elif rh_node.elem_type == "var":
            try:
                vname = rh_node.dict["name"]
                rh = local_vtable[vname]
            except KeyError:
                try:
                    rh = local_ftable[vname]
                except KeyError:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Variable/function '" + vname + "' not found")
                if len(rh) > 1:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Function '" + vname + "' has multiple overloaded versions")
                local_ftable[lh] = rh
                return None
        elif rh_node.elem_type == "lambda":
            rh = LambdaClosure(rh_node, local_vtable, local_ftable)
            local_ftable[lh] = rh
            return None
        elif rh_node.elem_type in Interpreter.__datatypes:
            rh = rh_node.dict["val"]
        elif rh_node.elem_type == "nil":
            rh = None
        elif rh_node.elem_type == "fcall":
            rh = self.eval_fcall(rh_node, local_vtable, local_ftable)
        else:
            print(f"rh_node.elem_type: {rh_node.elem_type}")
            print(f"rh value: {rh}")
            super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        if isinstance(rh, bool):
            rhv = BoolRef(rh)
        elif isinstance(rh, int):
            rhv = IntRef(rh)
        elif isinstance(rh, str):
            rhv = StrRef(rh)
        else:
            rhv = rh
        local_vtable[lh] = rhv
        return None

    def eval_binop(self, exp, local_vtable, local_ftable):
        op = [None, None]
        for i in [0,1]:
            opn = "op" + str(i+1)
            if exp.dict[opn].elem_type in Interpreter.__binops:
                op[i] = self.eval_binop(exp.dict[opn], local_vtable, local_ftable)
            elif exp.dict[opn].elem_type in Interpreter.__unops:
                op[i] = self.eval_unary(exp.dict[opn], local_vtable, local_ftable)
            elif exp.dict[opn].elem_type == "var":
                vname = exp.dict[opn].dict["name"]
                try:
                    opv = local_vtable[vname]
                except KeyError:
                    try:
                        opv = local_ftable[vname]
                    except KeyError:
                        super().error(intbase.ErrorType.NAME_ERROR,
                                      "Variable/function '" + vname + "' not found")
                op[i] = opv.val if isRefType(obv) else opv
            elif exp.dict[opn].elem_type in Interpreter.__datatypes:
                op[i] = exp.dict[opn].dict["val"]
            elif exp.dict[opn].elem_type == "nil":
                op[i] = None
            elif exp.dict[opn].elem_type == "fcall":
                op[i] = self.eval_fcall(exp.dict[opn], local_vtable, local_ftable)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you messed up")
        if exp.elem_type == '+':
            # Python automatically casts bools for us, so ez
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] + op[1]
            elif isinstance(op[0], str) and isinstance(op[1], str):
                return op[0] + op[1]
            else:
                return self.binop_error('+', op[0], op[1], local_ftable)
        elif exp.elem_type in ['-', '*', '/']:
            # more ez Python automatic bool casting to int
            if isinstance(op[0], int) and isinstance(op[1], int):
                if exp.elem_type == '-':
                    return op[0] - op[1]
                elif exp.elem_type == '*':
                    return op[0] * op[1]
                else:
                    return op[0] // op[1]
            else:
                return self.binop_error(exp.elem_type, op[0], op[1], local_ftable)
        elif exp.elem_type == "==":
            if isinstance(op[0], int) and isinstance(op[1], int):
                # catch behavior of == returning true for any non-zero
                # for (true == non-zero)
                if isinstance(op[0], bool):
                    if (op[0] == True):
                        return True if op[1] != 0 else False
                    else:
                        return op[0] == op[1]
                elif isinstance(op[1], bool):
                    if (op[1] == True):
                        return True if op[0] != 0 else False
                    else:
                        return op[0] == op[1]
                else:
                    return op[0] == op[1]
            elif op[0] in local_ftable.values() and op[1] in local_ftable.values():
                return op[0] is op[1]
            elif type(op[0]) == type(op[1]):
                return op[0] == op[1]
            else:
                return False
        elif exp.elem_type == "!=":
            if isinstance(op[0], int) and isinstance(op[1], int):
                if isinstance(op[0], bool):
                    if (op[0] == True):
                        return False if op[0] != 0 else True
                    else:
                        return op[0] != op[1]
                elif isinstance(op[1], bool):
                    if (op[1] == True):
                        return False if op[0] != 0 else True
                    else:
                        return op[0] != op[1]
                else:
                    return op[0] != op[1]
            elif op[0] in local_ftable.values() and op[1] in local_ftable.values():
                return op[0] is not op[1]
            elif type(op[0]) == type(op[1]):
                return op[0] != op[1]
            else:
                return True
        elif exp.elem_type in ['<', "<="]:
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] < op[1] if exp.elem_type == '<' else op[0] <= op[1]
            else:
                return self.binop_error(exp.elem_type, op[0], op[1], local_ftable)
        elif exp.elem_type in ['>', ">="]:
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] > op[1] if exp.elem_type == '>' else op[0] >= op[1]
        elif exp.elem_type in ["||", "&&"]:
            if isinstance(op[0], int) and isinstance(op[1], int):
                if exp.elem_type == "||":
                    return (op[0] + op[1]) > 0
                else:
                    return op[0] != 0 and op[1] != 0
            else:
                return self.binop_error(exp.elem_type, op[0], op[1], local_ftable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you messed up")

    def binop_error(self, op, a, b, local_ftable):
        if type(a) == type(b):
            if a in local_ftable.values() and b in local_ftable.values():
                super().error(intbase.ErrorType.TYPE_ERROR,
                              "Incompatible operator " + op + " for type Type.CLOSURE")
            elif isinstance(a, str):
                super().error(intbase.ErrorType.TYPE_ERROR,
                              "Incompatible operator " + op + " for type Type.STRING")
        else:
            super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible types for "+ op +" operation")

    def eval_unary(self, exp, local_vtable, local_ftable):
        if exp.dict["op1"].elem_type in Interpreter.__binops:
            op = self.eval_binop(exp.dict["op1"], local_vtable, local_ftable)
        elif exp.dict["op1"].elem_type in Interpreter.__unops:
            op = self.eval_unary(exp.dict["op1"], local_vtable, local_ftable)
        elif exp.dict["op1"].elem_type == "var":
            vname = exp.dict["op1"].dict["name"]
            if vname in local_ftable:
                super().error(intbase.ErrorType.TYPE_ERROR,
                              "Incompatible type for " + exp.elem_type + " operation")
            try:
                opv = local_vtable[vname]
            except KeyError:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Variable/function '" + vname "' not found")
            op = opv.val if isBoxType(opv) else opv
        elif exp.dict["op1"] in Interpreter.__datatypes:
            op = exp.dict["op1"].dict["val"]
        elif exp.dict["op1"].elem_type == "nil":
            op = None
        elif exp.dict["op1"].elem_type == "fcall":
            op = self.eval_fcall(exp.dict["op1"], local_vtable, local_ftable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you messed up")
        if exp.elem_type == "neg":
            if isinstance(op, int) and not isinstance(op, bool):
                return -(op)
            else:
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for neg operation")
        elif exp.elem_type == '!':
            if isinstance(op, int):
                if op != 0:
                    return False
                else:
                    return True
            else:
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for ! operation")
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "ya dun goofed")

    def eval_fcall(self, call, local_vtable, local_ftable):
        fname = call.dict["name"]
        if fname == "print":
            return self.eval_print(call, local_vtable, local_ftable)
        elif fname == "inputi":
            return self.eval_input(call, "int", local_vtable, local_ftable)
        elif fname == "inputs":
            return self.eval_input(call, "str", local_vtable, local_ftable)

        # Look for the correct function type signature
        try:
            sig = self.__ftable[fname]
        except KeyError:
            super().error(intbase.ErrorType.NAME_ERROR, "Function " + fname + "not found")
        f = None
        for g in sig:
            if len(g.dict["args"]) == len(call.dict["args"]):
                f = g
                break
        if f is None:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No " + fname + "() function that takes " +
                          str(len(call.dict["args"])) + " parameter(s)")
        vargs = []
            
            

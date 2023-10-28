from brewparse import parse_program
import intbase

class Interpreter(intbase.InterpreterBase):
    __binops = ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">="]
    __unops = ["neg", '!']
    __datatypes = ["int", "string", "bool"]
    
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self._vtable = dict()    # vtable for main()
        self._ftable = dict()

    def run(self, program):
        parsed_program = parse_program(program)
        if len(self._ftable) < 1:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No main() function was found")

        # Store a function table and a pointer to the main function
        found_main = False
        main = None
        for f in self._ftable:
            fname = f.dict["name"]
            if fname == "main":
                found_main = True
                main = f
            else:
                try:
                    keyexists = self._ftable[fname]
                except KeyError:
                    self._ftable[fname] = []
                self._ftable[fname].append(f)
        if found_main == False:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No main() function was found")
        for s in main.dict["statements"]:
            if s.elem_type == '=':
                self.eval_assign(s)
            elif s.elem_type == "fcall":
                self.eval_fcall(s)
            elif s.elem_type == "if":
                self.eval_if(s)
            elif s.elem_type == "while":
                self.eval_while(s)
            elif s.elem_type == "return":
                return
            
    def eval_assign(self, s, local_vtable=self._vtable):
        lh = s.dict["name"]
        rh_node = s.dict["expression"]
        rh = None
        if rh_node.elem_type in Interpreter.__binops:
            rh = self.eval_binop(rh_node, local_vtable)
        if rh_node.elem_type in Interpreter.__unops:
            rh = self.eval_unary(rh_node, local_vtable)
        elif rh_node.elem_type == "var":
            try:
                vname = rh_node.dict["name"]
                rh = local_vtable[vname]
            except KeyError:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Variable '" + vname + "' is not defined")
        elif rh_node.elem_type in Interpreter.__datatypes:
            rh = rh_node.dict["val"]
        elif rh_node.elem_type == "nil":
            rh = None
        elif rh_node.elem_type == "fcall":
            rh = self.eval_fcall(rh_node, local_vtable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        local_vtable[lh] = rh

    def eval_binop(self, exp, local_vtable=self._vtable):
        op = [None, None]
        for i in [0, 1]:
            opn = "op" + str(i)
            if exp.dict[opn].elem_type in Interpreter.__binops:
                op[i] = self.eval_binop(exp.dict[opn], local_vtable)
            elif exp.dict[opn].elem_type in Interpreter.__unops:
                op[i] = self.eval_unary(exp.dict[opn], local_vtable)
            elif exp.dict[opn].elem_type == "var":
                try:
                    vname = exp.dict[opn].dict["name"]
                    op[i] = local_vtable[vname]
                except KeyError:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Variable '" + vname + "' is not defined")
            elif exp.dict[opn].elem_type in ["int", "string", "bool"]:
                op[i] = exp.dict[opn].dict["val"]
            elif exp.dict[opn].elem_type == "nil":
                op[i] = None
            elif exp.dict[opn].elem_type == "fcall":
                op[i] = self.eval_fcall(exp.dict[opn], local_vtable)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you messed up")
        if exp.elem_type == '+':
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] + op[1]
            elif isinstance(op[0], str) and isinstance(op[1], str):
                return op[0] + op[1]
            else:
                Interpreter.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ['-', '*', '/']:
            if isinstance(op[0], int) and isinstance(op[1], int):
                if exp.elem_type == '-':
                    return op[0] - op[1]
                elif exp.elem_type == '*':
                    return op[0] * op[1]
                else:
                    return op[0] // op[1]
            else:
                Interpreter.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type == "==":
            if type(op[0]) == type(op[1]):
                return op[0] == op[1]
            else:
                return False
        elif exp.elem_type == "!=":
            if type(op[0]) == type(op[1]):
                return op[0] != op[1]
            else:
                return True
        elif exp.elem_type in ['<', "<="]:
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] < op[1] if exp.elem_type == '<' else op[0] <= op[1]
            else:
                Interpreter.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ['>', ">="]:
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] > op[1] if exp.elem_type == '>' else op[0] >= op[1]
            else:
                Interpreter.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ["||", "&&"]:
            if isinstance(op[0], bool) and isinstance(op[1], bool):
                return op[0] || op[1] if exp.elem_type == "||" else op[0] && op[1]
            else:
                Interpreter.binop_error(exp.elem_type, op[0], op[1])
        else:
            super().error(intbase.ErrorTYpe.NAME_ERROR, "you messed up")

    def binop_error(op, a, b):
        if type(a) == type(b):
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible operator " + op + " for type Type." +
                          str(a.__class__.__name__).upper())
        else:
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible types for " + op + " operation")
            
    def eval_unary(self, exp, local_vtable=self._vtable):
        if exp.dict["op1"].elem_type in ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">=",
                                         "||", "&&"]:
            op = self.eval_binop(exp.dict["op1"], local_vtable)
        elif exp.dict["op1"].elem_type in ["neg", '!']:
            op = self.eval_unary(exp.dict["op1"], local_vtable)
        elif exp.dict["op1"].elem_type == "var":
            try:
                vname = exp.dict["op1"].dict["name"]
                op = local_vtable[vname]
            except KeyError:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Variable '" + vname + "' is not defined")
        elif exp.dict["op1"].elem_type in ["int", "string", "bool"]:
            op = exp.dict["op1"].dict["val"]
        elif exp.dict["op1"].elem_type == "nil":
            op = none
        elif exp.dict["op1"].elem_type == "fcall":
            op = self.eval_fcall(exp.dict["op1"], local_vtable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you messed up")
        if exp.elem_type == "neg":
            if isinstance(op, int):
                return -(op)
            else:
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for neg operation")
        elif exp.elem_type == '!':
            if isinstance(op, bool):
                return !op
            else:
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for ! operation")
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you messed up")

    def eval_fcall(self, call, local_vtable=self._vtable):
        fname = call.dict["name"]
        if fname == "print":
            return self.eval_print(call, local_vtable)
        elif fname == "inputi":
            return self.eval_inputi(call, local_vtable)
        elif fname == "inputs":
            return self.eval_inputs(call, local_vtable)
        try:
            sig = local_vtable[fname]
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
        args = []
        for arg in call.dict["args"]:
            if arg.elem_type in Interpreter.__binops:
                args.append(self.eval_binop(arg, local_vtable))
            elif arg.elem_type in Interpreter.__unops:
                args.append(self.eval_unary(arg, local_vtable))
            elif arg.elem_type == "var":
                try:
                    vname = arg.dict["name"]
                    vval = local_vtable[vname]
                except KeyError:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Variable '" + vname + "' is not defined")
                args.append(vval)
            elif arg.elem_type in Interpreter.__datatypes:
                args.append(arg.dict["val"])
            elif arg.elem_type == "nil":
                args.append(None)
            elif arg.elem_type == "fcall":
                args.append(self.eval_fcall(arg, local_vtable))
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        return self.eval_fdef(f, args)

    def eval_fdef(self, f, args):
        local_vtable = dict()
        for i in range(len(args)):
            vname = f.dict["args"][i].dict["name"]
            local_vtable[vname] = args[i]
        for s in f.dict["statements"]:
            if s.elem_type == 

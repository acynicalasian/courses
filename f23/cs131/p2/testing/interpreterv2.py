from brewparse import parse_program
import intbase
import copy

class Interpreter(intbase.InterpreterBase):
    __binops = ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">="]
    __unops = ["neg", '!']
    __datatypes = ["int", "string", "bool"]
    
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.__vtable = dict()    # vtable for main()
        self.__ftable = dict()

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
                self.eval_assign(s, self.__vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, self.__vtable)
            elif s.elem_type == "if":
                self.eval_if(s, copy.deepcopy(self.__vtable))
            elif s.elem_type == "while":
                self.eval_while(s, copy.deepcopy(self.__vtable))
            elif s.elem_type == "return":
                return self.eval_return(s, self.__vtable)
        return 0
            
    def eval_assign(self, s, local_vtable):
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

    def eval_binop(self, exp, local_vtable):
        op = [None, None]
        for i in [0, 1]:
            opn = "op" + str(i+1) 
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
            elif exp.dict[opn].elem_type in Interpreter.__datatypes:
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
                self.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ['-', '*', '/']:
            if isinstance(op[0], int) and isinstance(op[1], int):
                if exp.elem_type == '-':
                    return op[0] - op[1]
                elif exp.elem_type == '*':
                    return op[0] * op[1]
                else:
                    return op[0] // op[1]
            else:
                self.binop_error(exp.elem_type, op[0], op[1])
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
                self.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ['>', ">="]:
            if isinstance(op[0], int) and isinstance(op[1], int):
                return op[0] > op[1] if exp.elem_type == '>' else op[0] >= op[1]
            else:
                self.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ["||", "&&"]:
            if isinstance(op[0], bool) and isinstance(op[1], bool):
                return op[0] or op[1] if exp.elem_type == "||" else op[0] and op[1]
            else:
                self.binop_error(exp.elem_type, op[0], op[1])
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you messed up")

    def binop_error(self, op, a, b):
        if type(a) == type(b):
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible operator " + op + " for type Type." +
                          str(a.__class__.__name__).upper())
        else:
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible types for " + op + " operation")
            
    def eval_unary(self, exp, local_vtable):
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
            op = None
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
                return not op
            else:
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for ! operation")
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you messed up")

    def eval_fcall(self, call, local_vtable):
        fname = call.dict["name"]
        if fname == "print":
            return self.eval_print(call, local_vtable)
        elif fname == "inputi":
            return self.eval_input(call, "int", local_vtable)
        elif fname == "inputs":
            return self.eval_input(call, "str", local_vtable)
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
        scope_vtable = copy.deepcopy(self.__vtable)
        for key in list(local_vtable):
            scope_vtable[key] = local_vtable[key]
        for s in f.dict["statements"]:
            if s.elem_type == '=':
                self.eval_assign(s, scope_vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, scope_vtable)
            elif s.elem_type == "if":
                self.eval_if(s, scope_vtable)
            elif s.elem_type == "while":
                self.eval_while(s, scope_vtable)
            elif s.elem_type == "return":
                return self.eval_return(s, scope_vtable)
        return None

    def eval_print(self, f, local_vtable):
        if len(f.dict["args"]) == 0:
            super().output("")
            return None
        acc = ""
        for arg in f.dict["args"]:
            if arg.elem_type in Interpreter.__binops:
                acc += str(self.eval_binop(arg, local_vtable))
            elif arg.elem_type == "nil":
                acc += "nil"
            else:
                if arg.elem_type in Interpreter.__unops:
                    aval = self.eval_unary(arg, local_vtable)
                elif arg.elem_type == "var":
                    try:
                        vname = arg.dict["name"]
                        aval = local_vtable[vname]
                    except KeyError:
                        super().error(intbase.ErrorType.NAME_ERROR,
                                      "Variable '" + vname + "' is not defined")
                elif arg.elem_type in Interpreter.__datatypes:
                    aval = arg.dict["val"]
                elif arg.elem_type == "fcall":
                    aval = self.eval_fcall(arg, local_vtable)
                else:
                    super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")
                if isinstance(aval, int):
                    acc += str(aval)
                elif isinstance(aval, bool):
                    if aval:
                        acc += "true"
                    else:
                        acc += "false"
                else:
                    acc += aval
        super().output(acc)
        return None

    def eval_input(self, f, itype, local_vtable):
        if len(f.dict["args"]) > 1:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No inputi() function that takes > 1 parameter")
        if len(f.dict["args"]) == 1:
            prompt = f.dict["args"][0]
            if prompt.elem_type in Interpreter.__binops:
                super().output(str(self.eval_binop(prompt, local_vtable)))
            elif prompt.elem_type == "nil":
                super().output("nil")
            else:
                if prompt.elem_type in Interpreter.__unops:
                    pval = self.eval_unary(prompt, local_vtable)
                elif prompt.elem_type == "var":
                    try:
                        vname = prompt.dict["name"]
                        pval = local_vtable[vname]
                    except KeyError:
                        super().error(intbase.ErrorType.NAME_ERROR,
                                      "Variable '" + vname + "' is not defined")
                elif prompt.elem_type in Interpreter.__datatypes:
                    pval = prompt.dict["val"]
                elif prompt.elem_type == "fcall":
                    pval = self.eval_fcall(prompt, local_vtable)
                else:
                    super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")
                if isinstance(pval, int):
                    super().output(str(pval))
                elif isinstance(pval, bool):
                    if pval:
                        super().output("true")
                    else:
                        super().output("false")
                else:
                    super().output(pval)
        if itype == "int":
            return int(super().get_input())
        else:
            return str(super().get_input())

    def eval_return(self, r, local_vtable):
        exp = r.dict["expression"]
        if exp is None:
            return None
        if exp.elem_type in Interpreter.__binops:
            out = self.eval_binop(exp, local_vtable)
        elif exp.elem_type == "nil":
            return None
        else:
            if exp.elem_type in Interpreter.__unops:
                out = self.eval_unary(exp, local_vtable)
            elif exp.elem_type == "var":
                try:
                    vname = exp.dict["name"]
                    out = local_vtable[vname]
                except KeyError:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Variable '" + vname + "' is not defined")
            elif exp.elem_type in Interpreter.__datatypes:
                out = exp.dict["val"]
            elif exp.elem_type == "fcall":
                out = self.eval_fcall(exp, local_vtable)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")
        return out

    def eval_if(self, chk, local_vtable):
        arg = chk.dict["condition"]
        if arg.elem_type in Interpreter.__binops:
            cond = self.eval_binop(arg, local_vtable)
        elif arg.elem_type == "nil":
            cond = None
        else:
            if arg.elem_type in Interpreter.__unops:
                cond = self.eval_unary(arg, local_vtable)
            elif arg.elem_type == "var":
                try:
                    vname = arg.dict["name"]
                    cond = local_vtable[vname]
                except KeyError:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Variable '" + vname + "' is not defined")
            elif arg.elem_type in Interpreter.__datatypes:
                cond = arg.dict["val"]
            elif arg.elem_type == "fcall":
                cond = self.eval_fcall(arg, local_vtable)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")
            if not isinstance(cond, bool):
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for if condition")
        if cond:
            for s in chk.dict["statements"]:
                if s.elem_type == '=':
                    self.eval_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.eval_fcall(s, local_vtable)
                elif s.elem_type == "if":
                    self.eval_if(s, copy.deepcopy(local_vtable))
                elif s.elem_type == "while":
                    self.eval_while(s, copy.deepcopy(local_vtable))
                elif s.elem_type == "return":
                    out = self.eval_return(s, local_vtable)
                    for key in list(local_vtable):
                        if key in self.__vtable:
                            self.__vtable[key] = local_vtable[key]
                    return out
            for key in list(local_vtable):
                if key in self.__vtable:
                    self.__vtable[key] = local_vtable[key]
                return
        else:
            if chk.dict["else_statements"] is None:
                return
            for s in chk.dict["else_statements"]:
                if s.elem_type == '=':
                    self.eval_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.eval_fcall(s, local_vtable)
                elif s.elem_type == "if":
                    self.eval_if(s, local_vtable)
                elif s.elem_type == "while":
                    self.eval_while(s, local_vtable)
                elif s.elem_type == "return":
                    return self.eval_return(s, local_vtable)
            return

    def eval_while(self, chk, local_vtable):
        arg = chk.dict["condition"]
        if arg.elem_type in Interpreter.__binops:
            cond = self.eval_binop(arg, local_vtable)
        elif arg.elem_type == "nil":
            cond = None
        else:
            if arg.elem_type in Interpreter.__unops:
                cond = self.eval_unary(arg, local_vtable)
            elif arg.elem_type == "var":
                try:
                    vname = arg.dict["name"]
                    cond = local_vtable[vname]
                except KeyError:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "Variable '" + vname + "' is not defined")
            elif arg.elem_type in Interpreter.__datatypes:
                cond = arg.dict["val"]
            elif arg.elem_type == "fcall":
                cond = self.eval_fcall(arg, local_vtable)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")
            if not isinstance(cond, bool):
                super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for if condition")
        if cond:
            for s in chk.dict["statements"]:
                if s.elem_type == '=':
                    self.eval_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.eval_fcall(s, local_vtable)
                elif s.elem_type == "if":
                    self.eval_if(s, local_vtable)
                elif s.elem_type == "while":
                    self.eval_while(s, local_vtable)
                elif s.elem_type == "return":
                    out = self.eval_return(s, local_vtable)
                    for key in local_vtable:
                        if key in self.__vtable:
                            self.__vtable[key] = local_vtable[key]
                    return out
            self.eval_while(chk, local_vtable)
        else:
            for key in local_vtable:
                if key in self.__vtable:
                    self.__vtable[key] = local_vtable[key]

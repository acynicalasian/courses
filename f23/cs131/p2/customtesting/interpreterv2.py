from brewparse import parse_program
import intbase
import copy

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
                self.eval_assign(s, self.__vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, self.__vtable)
            elif s.elem_type == "if":
                self.eval_if(s, self.__vtable, self.__vtable, [])
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["value"]
            elif s.elem_type == "while":
                self.eval_while(s, self.__vtable, self.__vtable, [])
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["value"]
            elif s.elem_type == "return":
                return self.eval_return(s, self.__vtable)
        return 0
            
    def eval_assign(self, s, local_vtable):
        lh = s.dict["name"]
        rh_node = s.dict["expression"]
        rh = None
        if rh_node.elem_type in self.__binops:
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
        # else:
        #     print(f"rh_node.elem_type: {rh_node.elem_type}")
        #     print(f"rh value: {rh}")
        #     super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        local_vtable[lh] = rh
        return None

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
            if (type(op[0]) == type(op[1]) and (isinstance(op[0], str) or isinstance(op[0], int))
                and not isinstance(op[0], bool)):
                return op[0] + op[1]
            else:
                self.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ['-', '*', '/']:
            if type(op[0]) == type(op[1]) and isinstance(op[0], int) and not isinstance(op[0],bool):
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
            if type(op[0]) == type(op[1]) and isinstance(op[0],int) and not isinstance(op[0],bool):
                return op[0] < op[1] if exp.elem_type == '<' else op[0] <= op[1]
            else:
                self.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type in ['>', ">="]:
            if type(op[0]) == type(op[1]) and isinstance(op[0],int) and not isinstance(op[0],bool):
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
            if isinstance(op, int) and not isinstance(op, bool):
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

        # Look for the correct function type signature
        try:
            sig = self.__ftable[fname]
        except KeyError:
            super().error(intbase.ErrorType.NAME_ERROR, "Function " + fname + " not found")
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
        return self.eval_fdef(f, args, copy.deepcopy(local_vtable), local_vtable)

    def eval_fdef(self, f, args, local_vtable, original_vtable):
        shadowed_vtable = dict()
        block_vtable = []
        for i in range(len(args)):
            vname = f.dict["args"][i].dict["name"]
            shadowed_vtable[vname] = args[i]
        for key in list(shadowed_vtable):
            local_vtable[key] = shadowed_vtable[key]
        # I'll be honest, this code is so spaghetti that I'm not sure if this loop actually does
        # anything
        for key in list(original_vtable):    # realign the deep copy, except shadowed vars
            if key not in list(shadowed_vtable):
                local_vtable[key] = original_vtable[key]
        for s in f.dict["statements"]:
            if s.elem_type == '=':
                vname = s.dict["name"]
                # dynamically in scope, not shadowed
                if vname not in list(shadowed_vtable) and vname in list(original_vtable):
                    self.eval_assign(s, local_vtable)
                    original_vtable[vname] = local_vtable[vname]
                # function scoped (changes "disappear" because it's on a deep copy)
                # or it's shadowed
                else:
                    self.eval_assign(s, local_vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, local_vtable)
            elif s.elem_type == "if":
                self.eval_if(s, local_vtable, original_vtable, list(shadowed_vtable))
                if self.__nestedbreak["flag"]:
                    out = self.__nestedbreak["value"]
                    self.__nestedbreak["flag"] = False
                    self.__nestedbreak["value"] = None
                    return out
            elif s.elem_type == "while":
                self.eval_while(s, local_vtable, original_vtable, list(shadowed_vtable))
                if self.__nestedbreak["flag"]:
                    out = self.__nestedbreak["value"]
                    self.__nestedbreak["flag"] = False
                    self.__nestedbreak["value"] = None
                    return out
            elif s.elem_type == "return":
                return self.eval_return(s, local_vtable)            
        return None

    def eval_print(self, f, local_vtable):
        if len(f.dict["args"]) == 0:
            super().output("")
            return None
        acc = ""
        for arg in f.dict["args"]:
            if arg.elem_type in Interpreter.__binops:
                bop = self.eval_binop(arg, local_vtable)
                if bop and isinstance(bop, bool):
                    acc += "true"
                elif not bop and isinstance(bop, bool):
                    acc += "false"
                else:
                    acc += str(bop)
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
                if isinstance(aval, bool):
                    if aval:
                        acc += "true"
                    else:
                        acc += "false"
                elif isinstance(aval, int):
                    acc += str(aval)
                elif aval is None:
                    acc += "nil"
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
                if isinstance(pval, bool):
                    if pval:
                        super().output("true")
                    else:
                        super().output("false")
                elif isinstance(pval, int):
                    super().output(str(pval))
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
            # else:
            #     super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")
        return out

    def eval_if(self, chk, local_vtable, original_vtable, shadowed_vtable):
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

        block_vtable = []                
        if cond:
            for s in chk.dict["statements"]:
                if self.__nestedbreak["flag"]:
                    for vname in block_vtable:
                        if vname in list(local_vtable):
                            del local_vtable[vname]
                    return None
                if s.elem_type == '=':
                    # scoped within if-block
                    vname = s.dict["name"]
                    if vname not in list(local_vtable):
                        block_vtable.append(s.dict["name"])
                        self.eval_assign(s, local_vtable)
                    # dynamically in scope, not shadowed
                    elif vname not in shadowed_vtable and vname in list(original_vtable):
                        self.eval_assign(s, local_vtable)
                        original_vtable[vname] = local_vtable[vname]
                    else:                        
                        self.eval_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.eval_fcall(s, local_vtable)
                elif s.elem_type == "if":
                    self.eval_if(s, local_vtable, original_vtable, shadowed_vtable)
                    if self.__nestedbreak["flag"]:
                        out = self.__nestedbreak["value"]
                        self.__nestedbreak
                elif s.elem_type == "while":
                    self.eval_while(s, local_vtable, original_vtable, shadowed_vtable)
                elif s.elem_type == "return":
                    out = self.eval_return(s, local_vtable)
                    for vname in block_vtable:
                        del local_vtable[vname]
                    self.__nestedbreak["flag"] = True
                    self.__nestedbreak["value"] = out
                    return out
            for vname in block_vtable:
                del local_vtable[vname]
            return None
        else:
            if chk.dict["else_statements"] is None:
                return None
            for s in chk.dict["else_statements"]:
                if self.__nestedbreak["flag"]:
                    for vname in block_vtable:
                        if vname in list(local_vtable):
                            del local_vtable[vname]
                    return None
                if s.elem_type == '=':
                    # scoped within else-block
                    vname = s.dict["name"]
                    if vname not in list(local_vtable):
                        block_vtable.append(s.dict["name"])
                        self.eval_assign(s, local_vtable)
                    # dynamically in scope, not shadowed
                    elif vname not in shadowed_vtable and vname in list(original_vtable):
                        self.eval_assign(s, local_vtable)
                        original_vtable[vname] = local_vtable[vname]
                    else:
                        self.eval_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.eval_fcall(s, local_vtable)
                elif s.elem_type == "if":
                    self.eval_if(s, local_vtable, original_vtable, shadowed_vtable)
                elif s.elem_type == "while":
                    self.eval_while(s, local_vtable, original_vtable, shadowed_vtable)
                elif s.elem_type == "return":
                    out = self.eval_return(s, local_vtable)
                    for vname in block_vtable:
                        del local_vtable[vname]
                    self.__nestedbreak["flag"] = True
                    self.__nestedbreak["value"] = out
                    return out
            for vname in block_vtable:
                del local_vtable[vname]
            return None

    def eval_while(self, chk, local_vtable, original_vtable, shadowed_vtable):
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
            super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for while condition")

        block_vtable = []
        if cond:
            for s in chk.dict["statements"]:
                if self.__nestedbreak["flag"]:
                    for vname in block_vtable:
                        if vname in list(local_vtable):
                            del local_vtable[vname]
                    return None
                if s.elem_type == '=':
                    # scoped within while-block
                    vname = s.dict["name"]
                    if vname not in list(local_vtable):
                        block_vtable.append(s.dict["name"])
                        self.eval_assign(s, local_vtable)
                    # dynamically in scope, not shadowed
                    elif vname not in shadowed_vtable and vname in list(original_vtable):
                        self.eval_assign(s, local_vtable)
                        original_vtable[vname] = local_vtable[vname]
                    else:
                        self.eval_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.eval_fcall(s, local_vtable)
                elif s.elem_type == "if":
                    self.eval_if(s, local_vtable, original_vtable, shadowed_vtable)
                    if self.__nestedbreak["flag"]:
                        out = self.__nestedbreak["value"]
                elif s.elem_type == "while":
                    self.eval_while(s, local_vtable, original_vtable, shadowed_vtable)
                elif s.elem_type == "return":
                    out = self.eval_return(s, local_vtable)
                    for vname in block_vtable:
                        del local_vtable[vname]
                    self.__nestedbreak["flag"] = True
                    self.__nestedbreak["value"] = out
                    return out
            for vname in block_vtable:
                if vname in list(local_vtable):
                    del local_vtable[vname]
            self.eval_while(chk, local_vtable, original_vtable, shadowed_vtable)
        else:
            return None

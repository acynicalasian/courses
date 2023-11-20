from brewparse import parse_program
import intbase
import copy

class Ref:
    def __init__(self, v):
        self.val = v

class Function:
    def __init__(self, f, local_vtable=None, ref_vtable=None):
        self.fdef = f.dict["statements"]
        self.args = f.dict["args"]
        self.closures = copy.deepcopy(local_vtable)
        self.scopedref = copy.deepcopy(ref_vtable)
        if self.closures is not None:
            for a in self.args:
                if a.dict["name"] in self.closures:
                    # Lambda parameters shadow closures
                    del self.closures[a.dict["name"]]

class Interpreter(intbase.InterpreterBase):
    __binops = ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">=", "||", "&&"]
    __unops = ["neg", '!']
    __datatypes = ["int", "string", "bool"]
    __elemtypes = __binops + __unops + __datatypes + ["nil", "fcall"]
    

    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.__vtable = dict()    # vtable for main()
        self.__nestedbreak = { "flag" : False, "val" : None }    # returning from inside if/whiles
        self.__fncontext = ""

    def run(self, program):
        prog = parse_program(program)
        if len(prog.dict["functions"]) < 1:
            super().error(intbase.ErrorType.NAME_ERROR, "No main() function was found")
        # Store a variable table and a pointer to the main function
        found_main = False
        main = None
        for f in prog.dict["functions"]:
            fname = f.dict["name"]
            if fname == "main":
                found_main = True
                main = f
            if fname not in self.__vtable:
                self.__vtable[fname] = []
            self.__vtable[fname].append(Function(f))
        if not found_main:
            super().error(intbase.ErrorType.NAME_ERROR, "No main() function was found")
        for s in main.dict["statements"]:
            self.__fncontext = "main"
            if s.elem_type == '=':
                self.eval_assign(s, self.__vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, self.__vtable)
            elif s.elem_type == "if":
                self.eval_if(s, self.__vtable)
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["val"]
            elif s.elem_type == "while":
                self.eval_while(s, self.__vtable)
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["val"]
            elif s.elem_type == "return":
                return self.eval_return(s, self.__vtable)
        return 0

    def eval_assign(self, s, local_vtable, closure_vtable=None, ref_vtable=None):
        #breakpoint()
        lh = s.dict["name"]
        rh_node = s.dict["expression"]
        rh = None
        if rh_node.elem_type in Interpreter.__elemtypes:
            # old line
            #rh = Ref(self.eval_elem(rh_node, local_vtable, closure_vtable))
            e = self.eval_elem(rh_node, local_vtable, closure_vtable, ref_vtable)
            if isinstance(e, Function):
                if e.closures is None:
                    rh = e
                else:
                    rh = copy.deepcopy(e)
            else:
                rh = Ref(e)
        elif rh_node.elem_type == "var":
            vname = rh_node.dict["name"]
            rh = self.get_var(vname, local_vtable, closure_vtable)
        elif rh_node.elem_type == "lambda":
            rh = Function(rh_node, local_vtable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        target_tables = [local_vtable]
        if closure_vtable is not None:
            if local_vtable is closure_vtable:
                pass
            elif lh in closure_vtable and local_vtable[self.__fncontext].closures is closure_vtable:
                target_tables.append(closure_vtable)
        for t in target_tables:
            if isinstance(rh, Function):
                t[lh] = rh
            else:
                if ref_vtable is not None:
                    if lh in ref_vtable:
                        t[lh].val = rh.val
                else:
                    t[lh] = Ref(rh.val)
        return None

    def get_var(self, vname, local_vtable, closure_vtable=None):
        if closure_vtable is not None:
            if vname in closure_vtable:
                out = closure_vtable[vname]
            else:
                out = local_vtable[vname]
        elif vname in local_vtable:
            out = local_vtable[vname]
        else:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "Variable/function " + vname + " not found")
        if isinstance(out, list):
            # Overload error takes precedence, looks like
            # Guaranteed to be a function if vtable entry is a list
            if len(out) > 1:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Function " + vname + " has multiple overloaded versions")
            else:
                return out[0]
        else:
            return out

    def eval_elem(self, e, local_vtable, closure_vtable=None, ref_vtable=None):
        if e.elem_type in Interpreter.__binops:
            return self.eval_binop(e, local_vtable, closure_vtable, ref_vtable)
        elif e.elem_type in Interpreter.__unops:
            return self.eval_unary(e, local_vtable, closure_vtable, ref_vtable)
        elif e.elem_type in Interpreter.__datatypes:
            return e.dict["val"]
        elif e.elem_type == "nil":
            return None
        elif e.elem_type == "fcall":
            return self.eval_fcall(e, local_vtable, closure_vtable, ref_vtable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")

    def eval_binop(self, exp, local_vtable, closure_vtable=None, ref_vtable=None):
        op = [None, None]
        for i in [0, 1]:
            opn = "op" + str(i+1)
            if exp.dict[opn].elem_type in Interpreter.__elemtypes:
                op[i] = self.eval_elem(exp.dict[opn], local_vtable, closure_vtable, ref_vtable)
            elif exp.dict[opn].elem_type == "var":
                vname = exp.dict[opn].dict["name"]
                v = self.get_var(vname, local_vtable, closure_vtable)
                op[i] = v if isinstance(v, Function) else v.val
            elif exp.dict[opn].elem_type == "lambda":
                op[i] = Function(exp.dict[opn], local_vtable, ref_vtable)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        if isinstance(op[0], Function) or isinstance(op[1], Function):
            if exp.elem_type == "==":
                return op[0] is op[1] if type(op[0]) == type(op[1]) else False
            elif exp.elem_type == "!=":
                return op[0] is not op[1] if type(op[0]) == type(op[1]) else True
            else:
                return self.binop_error(exp.elem_type, op[0], op[1])
        op = [ "nil" if elem is None else elem for elem in op ]
        
        if exp.elem_type == '+':
            # Python automatically casts bools to ints :)
            if ((isinstance(op[0], int) and isinstance(op[1], int)) or
                (isinstance(op[0], str) and isinstance(op[1], str))):
                return op[0] + op[1]
            else:
                return self.binop_error('+', op[0], op[1])
        elif exp.elem_type in ['-', '*', '/']:
            if isinstance(op[0], int) and isinstance(op[1], int):
                if exp.elem_type == '-':
                    return op[0] - op[1]
                elif exp.elem_type == '*':
                    return op[0] * op[1]
                else:
                    return op[0] // op[1]
            else:
                return self.binop_error(exp.elem_type, op[0], op[1])
        elif exp.elem_type == "==":
            if isinstance(op[0], int) and isinstance(op[1], int):
                # catch behavior of == returning true for (true == non-zero)
                if isinstance(op[0], bool) or isinstance(op[1], bool):
                    return op[0] != 0 and op[1] != 0
            return op[0] == op[1]
        elif exp.elem_type == "!=":
            return op[0] != op[1]
        elif isinstance(op[0], int) and isinstance(op[1], int):
            if exp.elem_type in ['<', "<="]:
                return op[0] < op[1] if exp.elem_type == '<' else op[0] <= op[1]
            elif exp.elem_type in ['>', ">="]:
                return op[0] > op[1] if exp.elem_type == '>' else op[0] >= op[1]
            else:
                if exp.elem_type == "||":
                    return op[0] != 0 or op[1] != 0
                else:
                    return op[0] != 0 and op[1] != 0
        else:
            return self.binop_error(exp.elem_type, op[0], op[1])

    def binop_error(self, op, a, b):
        if type(a) == type(b):            
            t = "TYPE.CLOSURE" if isinstance(a, Function) else "TYPE.STRING"
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible operator " + op + " for type " + t)
        else:
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible types for " + op + " operation")

    def eval_unary(self, exp, local_vtable, closure_vtable=None, ref_vtable=None):
        if exp.dict["op1"].elem_type in Interpreter.__elemtypes:
            op = self.eval_elem(exp.dict["op1"], local_vtable, closure_vtable)
        elif exp.dict["op1"].elem_type == "var":
            vname = exp.dict["op1"].dict["name"]
            op = self.get_var(vname, local_vtable, closure_vtable).val
        elif exp.dict["op1"].elem_type == "lambda":
            op = None
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")
        if exp.elem_type == "neg":
            if isinstance(op, int) and not isinstance(op, bool):
                return -(op)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "Incompatible type for neg operation")
        elif exp.elem_type == '!':
            if isinstance(op, int):
                return True if op == 0 else False
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "Incompatible type for ! operation")
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "ya dun goofed")
    
    def eval_fcall(self, call, local_vtable, closure_vtable=None, ref_vtable=None):
        last_context = self.__fncontext
        fname = call.dict["name"]
        if fname == "print":
            return self.eval_print(call, local_vtable, closure_vtable, ref_vtable)
        elif fname == "inputi":
            return self.eval_input(call, "int", local_vtable, closure_vtable, ref_vtable)
        elif fname == "inputs":
            return self.eval_input(call, "str", local_vtable, closure_vtable, ref_vtable)

        try:
            sig = local_vtable[fname]
        except KeyError:
            super().error(intbase.ErrorType.NAME_ERROR, "Function " + fname + " not found")
        if isinstance(sig, Function):
            f = sig
        else:
            f = None
            if not isinstance(sig, list):
                # Implicitly not a function (we caught lambdas in the if-statement above)
                super().error(intbase.ErrorType.TYPE_ERROR,
                              "Trying to call function with non-closure")
            for g in sig:
                if len(g.args) == len(call.dict["args"]):
                    f = g
                    break
            if f is None:
                # Implies it's a lambda
                if sig[0].closures is not None:
                    super().error(intbase.ErrorType.TYPE_ERROR, "Invalid # of args to lambda")
                else:
                    super().error(intbase.ErrorType.NAME_ERROR,
                                  "No " + fname + " function that takes " +
                                  str(len(call.dict["args"])) + " parameter(s)")
                
        args = []
        for a in call.dict["args"]:
            if a.elem_type in Interpreter.__elemtypes:
                args.append(Ref(self.eval_elem(a, local_vtable, closure_vtable, ref_vtable)))
            elif a.elem_type == "var":
                vname = a.dict["name"]
                args.append(self.get_var(vname, local_vtable, closure_vtable))
            elif a.elem_type == "lambda":
                args.append(Function(a, local_vtable))
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")

        # Not sure if I need to combine the closure table with a copy of the local table here

        self.__fncontext = fname
        out = self.eval_fdef(f, args, copy.deepcopy(local_vtable), local_vtable, f.closures)
        self.__fncontext = last_context
        return out

    def eval_fdef(self, f, args, local_vtable, original_vtable, closure_vtable=None, r_table=None):
        shadowed_vtable = dict()
        block_vtable = []
        ref_vtable = r_table
        for i in range(len(args)):
            vname = f.args[i].dict["name"]
            if f.args[i].elem_type == "refarg":
                if ref_vtable is None:
                    ref_vtable = []
                ref_vtable.append(vname)
            shadowed_vtable[vname] = args[i]
        # Working on this area
        if ref_vtable is not None:
            for vname in ref_vtable:
                # If we have a function passed as reference, we need to do a shallow copy
                # otherwise, switch shadowed_vtable val with a deepcopy
                if isinstance(shadowed_vtable[vname], Function):
                    shadowed_vtable[vname] = copy.deepcopy(shadowed_vtable[vname])
        # ^^^^^^^^^^^^^^^^^^^^
        for key in shadowed_vtable:
            local_vtable[key] = shadowed_vtable[key]
            
        # Realign the local vtable with the original vtable, just in case?
        # Still not sure whether I need this, but will throw it in anyway
        for key in original_vtable:
            if key not in shadowed_vtable:
                local_vtable[key] = original_vtable[key]
        for s in f.fdef:
            if s.elem_type == '=':
                vname = s.dict["name"]
                # dynamically in scope, not shadowed
                if vname not in shadowed_vtable and vname in original_vtable:
                    # avoid touching OG value if var exists in both closure_vtable and og_vtable
                    if closure_vtable is not None:
                        if vname in closure_vtable:
                            self.eval_assign(s, closure_vtable, closure_vtable, ref_vtable)
                    else:
                        self.eval_assign(s, original_vtable, closure_vtable, ref_vtable)
                    # testing this one line below
                    self.eval_assign(s, local_vtable, closure_vtable, ref_vtable)
                # not shadowed, but not dynamically scoped: (testing)
                elif vname not in shadowed_vtable and vname not in original_vtable:
                    self.eval_assign(s, local_vtable, closure_vtable, ref_vtable)
                #elif closure_vtable is not None:
                    #if vname in closure_vtable:
                        #self.eval_assign(s, closure_vtable, closure_vtable, ref_vtable)
                else:
                    self.eval_assign(s, local_vtable, closure_vtable, ref_vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, local_vtable, closure_vtable)
            elif s.elem_type == "if" or s.elem_type == "while":
                if s.elem_type == "if":
                    self.eval_if(s, local_vtable, closure_vtable, ref_vtable)
                else:
                    self.eval_while(s, local_vtable, closure_vtable, ref_vtable)
                if self.__nestedbreak["flag"]:
                    out = self.__nestedbreak["val"]
                    self.__nestedbreak["flag"] = False
                    self.__nestedbreak["val"] = None
                    if isinstance(out, Function):
                        return out
                    else:
                        return Ref(out)
            elif s.elem_type == "return":
                return self.eval_return(s, local_vtable, closure_vtable, ref_vtable)
        return None

    def eval_print(self, f, local_vtable, closure_vtable=None, ref_vtable=None):
        if len(f.dict["args"]) == 0:
            super().output("")
            return None
        acc = ""
        for arg in f.dict["args"]:
            if arg.elem_type in Interpreter.__elemtypes:
                bop = self.eval_elem(arg, local_vtable, closure_vtable)
            elif arg.elem_type == "var":
                bop = self.get_var(arg.dict["name"], local_vtable, closure_vtable).val
                assert not isinstance(bop, Function), "Error, can't pass function as a print param"
            elif arg.elem_type == "lambda":
                assert False, "Error, can't pass function as a print param"
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
            if bop and isinstance(bop, bool):
                acc += "true"
            elif not bop and isinstance(bop, bool):
                acc += "false"
            elif bop is None:
                acc += "nil"
            else:
                acc += str(bop)
        super().output(acc)
        return None

    def eval_input(self, f, itype, local_vtable, closure_vtable=None, ref_vtable=None):
        if len(f.dict["args"]) > 1:
            iname = "inputi()" if itype == "int" else "inputs()"
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No " + iname + " function that takes > 1 parameter")
        if len(f.dict["args"]) == 1:
            prompt = f.dict["args"][0]
            if prompt.elem_type in Interpreter.__elemtypes:
                pval = self.eval_elem(prompt, local_vtable, closure_vtable)
            elif prompt.elem_type == "var":
                pval = self.get_var(prompt.dict["name"], local_vtable, closure_vtable).val
                assert not isinstance(pval, Function), "Error, can't pass function as a print param"
            elif prompt.elem_type == "lambda":
                assert False, "Error, can't pass function as a print param"
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
            if isinstance(pval, bool):
                if pval:
                    super().output("true")
                else:
                    super().output("false")
            elif pval is None:
                super().output("nil")
            else:
                super().output(str(pval))
        if itype == "int":
            return int(super().get_input())
        else:
            return str(super().get_input())

    def eval_return(self, r, local_vtable, closure_vtable=None, ref_vtable=None):
        exp = r.dict["expression"]
        if exp is None:
            return None
        elif exp.elem_type in Interpreter.__elemtypes:
            return self.eval_elem(exp, local_vtable, closure_vtable)
        elif exp.elem_type == "var":
            return self.get_var(exp.dict["name"], local_vtable, closure_vtable)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "smth is wrong")

    def eval_if(self, chk, local_vtable, closure_vtable=None, ref_vtable=None):
        cond = self.eval_cond(chk.dict["condition"], local_vtable, closure_vtable, ref_vtable)
        if cond:
            return self.eval_cond_stats(chk.dict["statements"], local_vtable, closure_vtable,
                                        ref_vtable)
        else:
            return self.eval_cond_stats(chk.dict["else-statements"], local_vtable, closure_vtable,
                                        ref_vtable)
    def eval_while(self, chk, local_vtable, closure_vtable=None, ref_vtable=None):
        cond = self.eval_cond(chk.dict["condition"], local_vtable, closure_vtable, ref_vtable)
        if cond:
            self.eval_cond_stats(chk.dict["statements"], local_vtable, closure_vtable, ref_vtable)
            self.eval_while(chk, local_vtable, closure_vtable, ref_vtable)
        else:
            return None

    def eval_cond(self, arg, local_vtable, closure_vtable=None, ref_vtable=None):
        if arg.elem_type in Interpreter.__elemtypes:
            cond = self.eval_elem(arg, local_vtable, closure_vtable, ref_vtable)
        elif arg.elem_type == "var":
            cond = self.get_var(arg.dict["name"], local_vtable, closure_vtable)
            cond = None if isinstance(cond, Function) else cond.val
        elif arg.elem_type == "lambda":
            cond = None
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "you missed a case")
        if not isinstance(cond, int):
            super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for if condition")
        else:
            return cond

    def eval_cond_stats(self, st, local_vtable, closure_vtable=None, ref_vtable=None):
        if st is None:
            return None
        block_vtable = []
        for s in st:
            if self.__nestedbreak["flag"]:
                for vname in block_vtable:
                    if vname in local_vtable:
                        del local_vtable[vname]
                return None
            if s.elem_type == '=':
                if s.dict["name"] not in local_vtable:
                    block_vtable.append(s.dict["name"])
                self.eval_assign(s, local_vtable, closure_vtable, ref_vtable)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, local_vtable, closure_vtable, ref_vtable)
            elif s.elem_type == "if":
                self.eval_if(s, local_vtable, closure_vtable, ref_vtable)
            elif s.elem_type == "while":
                self.eval_while(s, local_vtable, closure_vtable, ref_vtable)
            elif s.elem_type == "return":
                out = self.eval_return(s, local_vtable, closure_vtable, ref_vtable)
                for vname in block_vtable:
                    del local_vtable[vname]
                self.__nestedbreak["flag"] = True
                self.__nestedbreak["value"] = out
                return None
        for vname in block_vtable:
            if vname in local_vtable:
                del local_vtable[vname]
        return None
            

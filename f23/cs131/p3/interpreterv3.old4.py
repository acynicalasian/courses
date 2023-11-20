from brewparse import parse_program
import intbase
import copy

class Ref:
    def __init__(self, v):
        self.val = v
    def setval(self, v):
        self.val = v

class Function:
    __binops = ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">=", "||", "&&"]
    __unops = ["neg", '!']
    __datatypes = ["int", "string", "bool"]
    __elemtypes = __binops + __unops + __datatypes + ["nil"]
    
    def __init__(self, f, is_lambda=False, local_vtable=None):
        self.is_lambda = is_lambda
        self.fdef = f.dict["statements"]
        self.args = f.dict["args"]
        self.refargs = []
        self.closures = dict()

        # Don't form closures if it's shadowed by the formal parameters
        self.__shadowedargs = []
        for a in self.args:
            if a.elem_type == "refarg":
                self.refargs.append(a.dict["name"])
            self.__shadowedargs.append(a.dict["name"])

        if local_vtable is not None:
            for s in self.fdef:
                if s.elem_type == '=':
                    self.bfs_assign(s, local_vtable)
                elif s.elem_type == "fcall":
                    self.bfs_fcall(s, local_vtable)
                elif s.elem_type in ["if", "while"]:
                    self.bfs_condblock(s, local_vtable)
                elif s.elem_type == "return":
                    self.bfs_return(s, local_vtable)
                    
    def handle_var(self, vname, local_vtable):
        if vname in local_vtable and vname not in self.__shadowedargs:
            if isinstance(local_vtable[vname], Function):
                self.closures[vname] = copy.deepcopy(local_vtable[vname])
            elif isinstance(local_vtable[vname], Ref):
                self.closures[vname] = Ref(local_vtable[vname].val)
            else:
                # Implicitly, we tried to equal to an overloaded function
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Function " + vname + " has multiple overloaded versions")
            return
        return

    def bfs_assign(self, s, local_vtable):
        # Right-hand side is an expression:
        rh_node = s.dict["expression"]
        if rh_node.elem_type in Function.__binops:
            return self.bfs_op(2, rh_node, local_vtable)
        elif rh_node.elem_type in Function.__unops:
            return self.bfs_op(1, rh_node, local_vtable)
        elif rh_node.elem_type == "fcall":
            return self.bfs_fcall(rh_node, local_vtable)
        elif rh_node.elem_type == "var":
            vname = rh_node.dict["name"]
            return self.handle_var(vname, local_vtable)
        else:
            return

    def bfs_op(self, numOps, exp, local_vtable):
        for i in range(numOps):
            opn = "op" + str(i+1)
            if exp.dict[opn].elem_type in Function.__binops:
                self.bfs_binop(exp.dict[opn], local_vtable)
            elif exp.dict[opn].elem_type in Function.__unops:
                self.bfs_unary(exp.dict[opn], local_vtable)
            elif exp.dict[opn].elem_type == "fcall":
                self.bfs_fcall(exp.dict[opn], local_vtable)
            elif exp.dict[opn].elem_type == "var":
                vname = exp.dict[opn].dict["name"]
                self.handle_var(vname, local_vtable)
        return

    def bfs_fcall(self, f, local_vtable):
        for a in f.dict["args"]:
            if a.elem_type in Function.__binops:
                self.bfs_op(2, a, local_vtable)
            elif a.elem_type in Function.__unops:
                self.bfs_op(1, a, local_vtable)
            elif a.elem_type == "fcall":
                self.bfs_fcall(a, local_vtable)
            elif a.elem_type == "var":
                vname = a.dict["name"]
                self.handle_var(vname, local_vtable)
        return

    def bfs_condblock(self, block, local_vtable):
        cond = block.dict["condition"]
        if cond.elem_type in Function.__binops:
            self.bfs_op(2, cond, local_vtable)
        elif cond.elem_type in Function.__unops:
            self.bfs_op(1, cond, local_vtable)
        elif cond.elem_type == "fcall":
            self.bfs_fcall(cond, local_vtable)
        elif cond.elem_type == "var":
            vname = cond.dict["name"]
            self.handle_var(vname, local_vtable)
            
        st = block.dict["statements"]
        if block.elem_type == "if":
            if "else_statements" in block.dict:
                st += block.dict["else_statements"]
        for s in st:
            if s.elem_type == '=':
                self.bfs_assign(s, local_vtable)
            elif s.elem_type == "fcall":
                self.bfs_fcall(s, local_vtable)
            elif s.elem_type in ["if", "while"]:
                self.bfs_condblock(s, local_vtable)
            elif s.elem_type == "return":
                self.bfs_return(s, local_vtable)
        return
    
    def bfs_return(self, r, local_vtable):
        if "expression" in r.dict:
            exp = r.dict["expression"]
            if exp.elem_type in Function.__binops:
                return self.bfs_op(2, exp, local_vtable)
            elif exp.elem_type in Function.__unops:
                return self.bfs_op(1, exp, local_vtable)
            elif exp.elem_type == "fcall":
                return self.bfs_fcall(exp, local_vtable)
            elif exp.elem_type == "var":
                vname = exp.dict["name"]
                return self.handle_var(vname, local_vtable)
        return

class Interpreter(intbase.InterpreterBase):
    __binops = ['+', '-', '*', '/', "==", "!=", '<', "<=", '>', ">=", "||", "&&"]
    __unops = ["neg", '!']
    __datatypes = ["int", "string", "bool"]
    __elemtypes = __binops + __unops + __datatypes + ["nil"]
    
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self.__vtable = dict()    # vtable for main()
        self.__nestedbreak = { "flag" : False, "val" : None }    # returning from inside if/whiles

    def run(self, program):
        prog = parse_program(program)
        if len(prog.dict["functions"]) < 1:
            super().error(intbase.ErrorType.NAME_ERROR, "No main() function was found")
        # Store a function table and a pointer to the main function
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
            if s.elem_type == '=':
                self.eval_assign(s, self.__vtable, self.__vtable["main"])
            elif s.elem_type == "fcall":
                self.eval_fcall(s, self.__vtable, self.__vtable["main"])
            elif s.elem_type == "if":
                self.eval_if(s, self.__vtable, self.__vtable["main"])
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["val"]
            elif s.elem_type == "while":
                self.eval_while(s, self.__vtable, self.__vtable["main"])
                if self.__nestedbreak["flag"]:
                    return self.__nestedbreak["val"]
            elif s.elem_type == "return":
                return self.eval_return(s, self.__vtable, self.__vtable["main"])
        return 0
            
    def eval_assign(self, s, local_vtable, current_fn):
        # Track the current function we're in to disambiguate closure scope and whatnot
        # eval functions and get_ref will all take closures into account when generating a value,
        # but we still need to double check whether we're in a closure once we finally assign
        lh = s.dict["name"]
        rh_node = s.dict["expression"]
        # literals, operations, and function calls all return values (not references)
        # ofc, except returning a function (but that's a deepcopy so w/e)
        if rh_node.elem_type in Interpreter.__elemtypes:
            rh = Ref(self.eval_val(rh_node, local_vtable, current_fn))    # eval_val guaranteed to return non-fn
        elif rh_node.elem_type == "fcall":
            rh = self.eval_fcall(rh_node, local_vtable, current_fn)
        # Return a reference first, and we'll reassign as needed
        elif rh_node.elem_type == "var":
            rh = self.get_ref(rh_node, local_vtable, current_fn)
            if not isinstance(rh, Function):

                #DEBUGGING
                if isinstance(rh.val, Ref):
                    raise TracebackException()
                
                rh = Ref(rh.val)
        elif rh_node.elem_type == "lambda":
            rh = Function(rh_node, True, local_vtable)
        else:
            assert False, "you missed a case"

        # Are we in a closure? Assign to the closure as well
        target_vtables = [local_vtable]
        if current_fn is not self.__vtable["main"] and lh in current_fn.closures:
            target_vtables.append(current_fn.closures)            
        for t in target_vtables:
            # If we have a refarg, use mutators as needed
            if current_fn is not self.__vtable["main"] and lh in current_fn.refargs:
                if isinstance(rh, Function):
                    # I don't think we need to worry about lh if rh is a fn
                    # eval_fcall should return a deep copy of functions anyway
                    t[lh] = rh
                else:
                    t[lh].val = rh.val
            # Not a refarg, we can set directly to rh
            else:
                t[lh] = rh
        return

    def get_ref(self, r, local_vtable, current_fn):
        vname = r.dict["name"]
        # Are we in a function and we want a captured value?
        if current_fn is not self.__vtable["main"] and vname in current_fn.closures:
            out = current_fn.closures[vname]
        elif vname in local_vtable:
            out = local_vtable[vname]
        else:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "Variable/function " + vname + " not found")
        # We can only have a list for overloaded functions
        if isinstance(out, list):
            if len(out) > 1:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Function " + vname + " has multiple overloaded versions")
            else:
                # We can never alias an overloaded function
                return out[0]
        else:
            return out

    def eval_val(self, e, local_vtable, current_fn):
        if e.elem_type in Interpreter.__binops:
            return self.eval_op(2, e, local_vtable, current_fn)
        elif e.elem_type in Interpreter.__unops:
            return self.eval_op(1, e, local_vtable, current_fn)
        elif e.elem_type in Interpreter.__datatypes:
            return e.dict["val"]
        elif e.elem_type == "nil":
            return None
        else:
            assert False, "you missed a case"
        
    def eval_op(self, numops, exp, local_vtable, current_fn):
        op = [None for i in range(numops)]
        for i in range(numops):
            opn = "op" + str(i+1)
            if exp.dict[opn].elem_type in Interpreter.__elemtypes:
                op[i] = self.eval_val(exp.dict[opn], local_vtable, current_fn)
            elif exp.dict[opn].elem_type == "fcall":
                # Remember, this returns primitives or functions, so no need to dereference
                op[i] = self.eval_fcall(exp.dict[opn], local_vtable, current_fn)
            elif exp.dict[opn].elem_type == "var":
                op[i] = self.get_ref(exp.dict[opn], local_vtable, current_fn)
                # If the var doesn't refer to a function, we need to dereference
                if not isinstance(op[i], Function):
                    op[i] = op[i].val
            elif exp.dict[opn].elem_type == "lambda":
                op[i] = Function(exp.dict[opn], True, local_vtable)
            else:
                assert False, "you missed a case"

        # Handle function equality first
        if len(op) == 2 and (isinstance(op[0], Function) or isinstance(op[1], Function)):
            if exp.elem_type == "==":
                return op[0] is op[1] if type(op[0]) == type(op[1]) else False
            elif exp.elem_type == "!=":
                return op[0] is not op[1] if type(op[0]) == type(op[1]) else True
            else:
                return self.binop_error(exp.elem_type, op[0], op[1])
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
        elif len(op) == 2 and isinstance(op[0], int) and isinstance(op[1], int):
            if exp.elem_type in ['<', "<="]:
                return op[0] < op[1] if exp.elem_type == '<' else op[0] <= op[1]
            elif exp.elem_type in ['>', ">="]:
                return op[0] > op[1] if exp.elem_type == '>' else op[0] >= op[1]
            else:
                if exp.elem_type == "||":
                    return op[0] != 0 or op[1] != 0
                else:
                    return op[0] != 0 and op[1] != 0
        elif exp.elem_type == "neg":
            if isinstance(op[0], int) and not isinstance(op[0], bool):
                return -(op[0])
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "Incompatible type for neg operation")
        elif exp.elem_type == '!':
            if isinstance(op[0], int):
                return True if op[0] == 0 else False
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "Incompatible type for ! operation")
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

    def eval_fcall(self, call, local_vtable, current_fn):
        fname = call.dict["name"]
        if fname == "print":
            return self.eval_print(call, local_vtable, current_fn)
        elif fname == "inputi":
            return self.eval_input(call, "int", local_vtable, current_fn)
        elif fname == "inputs":
            return self.eval_input(call, "str", local_vtable, current_fn)

        # Look for the correct function type signature
        try:
            sig = local_vtable[fname]
        except KeyError:
            super().error(intbase.ErrorType.NAME_ERROR, "Function " + fname + "not found")
        if isinstance(sig, Function):
            f = sig
            if len(f.args) != len(call.dict["args"]):
                super().error(intbase.ErrorType.TYPE_ERROR, "Invalid # of args to lambda")
        else:
            f = None
            # We already caught lambdas/function aliases above; if we get a non-list, we just
            # tried to call a function using a non-closure
            if not isinstance(sig, list):
                super().error(intbase.ErrorType.TYPE_ERROR,
                              "Trying to call function with non-closure")
            for g in sig:
                if len(g.args) == len(call.dict["args"]):
                    f = g
                    break
            if f is None:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "No " + fname + "() function that takes " +
                              str(len(call.dict["args"])) + " parameter(s)")
        
        args = []
        copytable = copy.deepcopy(local_vtable)
        for a in call.dict["args"]:
            if a.elem_type in Interpreter.__elemtypes:
                args.append(Ref(self.eval_val(a, local_vtable, current_fn)))
            elif a.elem_type == "fcall":
                v = self.eval_fcall(a, local_vtable, current_fn)
                if not isinstance(v, Function):
                    args.append(Ref(v))
                else:
                    args.append(v)
            # We need to deep copy function parameters that aren't refargs
            elif a.elem_type == "var":
                ref = self.get_ref(a, local_vtable, current_fn)
                if a.dict["name"] in f.refargs:
                    args.append(ref)
                    copytable[a.dict["name"]] = ref
                else:
                    if isinstance(ref, Function):
                        args.append(copy.deepcopy(ref))
                    else:
                        args.append(Ref(ref.val))
            elif a.elem_type == "lambda":
                args.append(Function(a, True, local_vtable))
            else:
                assert False, "you missed a case"
                
        # I think we need to overwrite refargs in the deepcopy to the og ptrs

        return self.eval_fdef(f, args, copytable, local_vtable, f)

    def eval_fdef(self, f, args, local_vtable, original_vtable, current_fn):
        shadowed_vtable = dict()
        block_vtable = []
        # Align positional params
        for i in range(len(args)):
            vname = f.args[i].dict["name"]
            shadowed_vtable[vname] = args[i]
        # Overwrite local_vtable with shadowed vars (we still have closures in current_fn.closures)
        for key in list(shadowed_vtable):
            local_vtable[key] = shadowed_vtable[key]
        # Realign the local_vtable wit the original_vtable, just in case?
        # Not sure if I need this, but will throw it in anyway
        for key in list(original_vtable):
            if key not in list(shadowed_vtable):
                local_vtable[key] = original_vtable[key]
        for s in f.fdef:
            if s.elem_type == '=':
                vname = s.dict["name"]
                # If we're in a closure, we can modify local_vtable and the closure table, but
                # we can't touch the original
                if vname in current_fn.closures:
                    self.eval_assign(s, local_vtable, current_fn)
                # Variable was not captured: if it's dynamically scoped, we also modify the og table
                else:
                    if vname in original_vtable:
                        self.eval_assign(s, original_vtable, current_fn)
                    # Regardless, we still change the local value as well
                    self.eval_assign(s, local_vtable, current_fn)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, local_vtable, current_fn)
            elif s.elem_type == "if":
                self.eval_if(s, local_vtable, current_fn)
                if self.__nestedbreak["flag"]:
                    out = self.__nestedbreak["value"]
                    self.__nestedbreak["flag"] = False
                    self.__nestedbreak["value"] = None
                    return out
            elif s.elem_type == "while":
                self.eval_while(s, local_vtable)
                if self.__nestedbreak["flag"]:
                    out = self.__nestedbreak["value"]
                    self.__nestedbreak["flag"] = False
                    self.__nestedbreak["value"] = None
                    return out
            elif s.elem_type == "return":
                return self.eval_return(s, local_vtable)
        return None

    def eval_print(self, f, local_vtable, current_fn):
        if len(f.dict["args"]) == 0:
            super().output("")
            return None
        acc = ""
        print(f"#args: {len(f.dict["args"])}")
        for arg in f.dict["args"]:
            if arg.elem_type in Interpreter.__elemtypes:
                txt = self.eval_val(arg, local_vtable, current_fn)
            elif arg.elem_type == "var":
                txt = self.get_ref(arg, local_vtable, current_fn)
                if isinstance(txt, Function):
                    rickroll()
                else:
                    txt = txt.val
            elif arg.elem_type == "fcall":
                txt = self.eval_fcall(arg, local_vtable, current_fn)
                if isinstance(txt, Function):
                    rickroll()
            elif arg.elem_type == "lambda":
                rickroll()
            else:
                assert False, "you missed a case"
            if isinstance(txt, bool):
                acc += "true" if txt else "false"
            elif txt is None:
                acc += "nil"
            else:
                acc += str(txt)
        super().output(acc)
        return None

    def eval_input(self, f, itype, local_vtable, current_fn):
        if len(f.dict["args"]) > 1:
            iname = "inputi()" if itype == "int" else "inputs()"
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No " + iname + " function that takes > 1 parameter")
        if len(f.dict["args"]) == 1:
            prompt = f.dict["args"][0]
            if prompt.elem_type in Interpreter.__elemtypes:
                pval = self.eval_val(prompt, local_vtable, current_fn)
            elif prompt.elem_type == "var":
                pval = self.get_ref(prompt, local_vtable, current_fn)
                if isinstance(pval, Function):
                    rickroll()
                else:
                    prompt = pval.val
            elif prompt.elem_type == "fcall":
                pval = self.eval_fcall(prompt, local_vtable, current_fn)
                if isinstance(pval, Function):
                    rickroll()
            elif prompt.elem_type == "lambda":
                rickroll()
            else:
                assert False, "you missed a case"
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

    def eval_return(self, r, local_vtable, current_fn):
        exp = r.dict["expression"]
        if exp is None:
            return None
        elif exp.elem_type in Interpreter.__elemtypes:
            return self.eval_val(exp, local_vtable, current_fn)
        elif exp.elem_type == "var":
            out = self.get_ref(exp, local_vtable, current_fn)
            if isinstance(out, Function):
                return copy.deepcopy(out)
            else:
                return out.val
        else:
            assert False, "you missed a case"

    def eval_if(self, chk, local_vtable, current_fn):
        cond = self.eval_cond("if", chk.dict["condition"], local_vtable, current_fn)
        if cond:
            return self.eval_cond_stats(chk.dict["statements"], local_vtable, current_fn)
        else:
            return self.eval_cond_stats(chk.dict["else-statements"], local_vtable, current_fn)

    def eval_while(self, chk, local_vtable, current_fn):
        cond = self.eval_cond("while", chk.dict["condition"], local_vtable, current_fn)
        if cond:
            self.eval_cond_stats(chk.dict["statements"], local_vtable, current_fn)
            return self.eval_while(chk, local_vtable, current_fn)
        else:
            return none

    def eval_cond(self, i, arg, local_vtable, current_fn):
        if arg.elem_type in Interpreter.__elemtypes:
            cond = self.eval_val(arg, local_vtable, current_fn)
        elif arg.elem_type == "fcall":
            cond = self.eval_fcall(arg, local_vtable, current_fn)
        elif arg.elem_type == "var":
            cond = self.get_ref(arg, local_vtable, current_fn)
            cond = None if isinstance(cond, Function) else cond.val
        elif arg.elem_type == "lambda":
            cond = None
        else:
            assert False, "you missed a case"
        if not isinstance(cond, int):
            super().error(intbase.ErrorType.TYPE_ERROR, "Incompatible type for " + i + " condition")
        else:
            return cond

    def eval_cond_stats(self, st, local_vtable, current_fn):
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
                self.eval_assign(s, local_vtable, current_fn)
            elif s.elem_type == "fcall":
                self.eval_fcall(s, local_vtable, current_fn)
            elif s.elem_type == "if":
                self.eval_if(s, local_vtable, current_fn)
            elif s.elem_type == "while":
                self.eval_while(s, local_vtable, current_fn)
            elif s.elem_type == "return":
                out = self.eval_return(s, local_vtable, current_fn)
                for vname in block_vtable:
                    del local_vtable[vname]
                self.__nestedbreak["flag"] = True
                self.__nestedbreak["val"] = out
                return None
        for vname in block_vtable:
            if vname in local_vtable:
                del local_vtable[vname]
        return None

def rickroll():
    import webbrowser
    webbrowser.open("https://www.youtube.com/watch?reload=9&v=dQw4w9WgXcQ")
    assert False, "This is undefined behavior"

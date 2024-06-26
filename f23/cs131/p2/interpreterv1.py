from brewparse import parse_program
import intbase

class Interpreter(intbase.InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)
        self._vtable = dict()

    def run(self, program):
        parsed_program = parse_program(program)
        if len(parsed_program.dict["functions"]) > 1:
            # spec says program node will only have one child function node
            super().error(intbase.ErrorType.NAME_ERROR,
                          "Program must only have function main() defined.")
        main_node = parsed_program.dict["functions"][0]
        if main_node.dict["name"] != "main":
            super().error(intbase.ErrorType.NAME_ERROR,
                          "No main() function was found")
        for s in main_node.dict["statements"]:
            if s.elem_type == '=':
                self.eval_assign(s)
            elif s.elem_type == "fcall":
                self.eval_fcall(s)
            else:
                super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")
                

    def eval_assign(self, s):
        lh = s.dict["name"]
        rh_node = s.dict["expression"]
        rh = None
        if rh_node.elem_type in ['+', '-']:
            rh = self.eval_binop(rh_node, rh_node.elem_type)
        elif rh_node.elem_type == "var":
            try:
                vname = rh_node.dict["name"]
                rh = self._vtable[vname]
            except KeyError:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Variable '" + vname + "' is not defined")
        elif rh_node.elem_type in ["int", "string"]:
            rh = rh_node.dict["val"]
        elif rh_node.elem_type == "fcall":
            rh = self.eval_fcall(rh_node)
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")
        self._vtable[lh] = rh
        
    def eval_binop(self, exp, op):
        if exp.dict["op1"].elem_type in ['+', '-']:
            op1 = self.eval_binop(exp.dict["op1"], exp.dict["op1"].elem_type)
        elif exp.dict["op1"].elem_type == "var":
            try:
                vname = exp.dict["op1"].dict["name"]
                op1 = self._vtable[vname]
            except KeyError:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Variable '" + vname + "' is not defined")
        elif exp.dict["op1"].elem_type in ["int", "string"]:
            op1 = exp.dict["op1"].dict["val"]
        elif (exp.dict["op1"].elem_type == "fcall" and
              exp.dict["op1"].dict["name"] == "inputi"):
            op1 = self.eval_fcall(exp.dict["op1"])
        elif (exp.dict["op1"].elem_type == "fcall" and
              exp.dict["op1"].dict["name"] == "print"):
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Operand op1 is of invalid type void")
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")

        if exp.dict["op2"].elem_type in ['+', '-']:
            op2 = self.eval_binop(exp.dict["op2"], exp.dict["op2"].elem_type)
        elif exp.dict["op2"].elem_type == "var":
            try:
                vname = exp.dict["op2"].dict["name"]
                op2 = self._vtable[vname]
            except KeyError:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "Variable '" + vname + "' is not defined")
        elif exp.dict["op2"].elem_type in ["int", "string"]:
            op2 = exp.dict["op2"].dict["val"]
        elif (exp.dict["op2"].elem_type == "fcall" and
              exp.dict["op2"].dict["name"] == "inputi"):
            op2 = self.eval_fcall(exp.dict["op2"])
        elif (exp.dict["op2"].elem_type == "fcall" and
              exp.dict["op2"].dict["name"] == "print"):
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Operand op2 is of invalid type void")
        else:
            super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")

        if isinstance(op1, str) or isinstance(op2, str):
            super().error(intbase.ErrorType.TYPE_ERROR,
                          "Incompatible types for arithmetic operation")
        elif op == '-':
            return op1 - op2
        else:
            return op1 + op2

    def eval_fcall(self, f):
        if f.dict["name"] == "print":
            if len(f.dict["args"]) == 0:
                super().output("")
                return "nil"
            acc = ""
            for arg in f.dict["args"]:
                if arg.elem_type in ['+', '-']:
                    acc += str(self.eval_binop(arg, arg.elem_type))
                elif arg.elem_type == "var":
                    try:
                        vname = arg.dict["name"]
                        tmpv = self._vtable[vname]
                    except KeyError:
                        super().error(intbase.ErrorType.NAME_ERROR,
                                      "Variable '" + vname + "' is not defined")
                    acc += str(tmpv)
                elif arg.elem_type in ["int", "string"]:
                    acc += str(arg.dict["val"])
                elif arg.elem_type == "fcall" and arg.dict["name"] == "print":
                    super().error(intbase.ErrorType.TYPE_ERROR,
                                  """Argument to function 'print' cannot be of
                                  type void""")
                elif arg.elem_type == "fcall" and arg.dict["name"] == "inputi":
                    # I seriously doubt this would happen but...
                    acc += str(self.eval_fcall(arg))
                else:
                    super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")
            super().output(acc)
            return "nil"
        elif f.dict["name"] == "inputi":
            if len(f.dict["args"]) == 1:
                prompt = f.dict["args"][0]
                if prompt.elem_type in ['+', '-']:
                    super().output(
                        str(self.eval_binop(prompt, prompt.elem_type)))
                elif prompt.elem_type == "var":
                    try:
                        vname = prompt.dict["name"]
                        tmpv = self._vtable[vname]
                    except KeyError:
                        super().error(intbase.ErrorType.NAME_ERROR,
                                      "Variable '" + vname + "' is not defined")
                    super().output(str(tmpv))
                elif prompt.elem_type in ["int", "string"]:
                    super().output(str(prompt.dict["val"]))
                elif prompt.elem_type == "fcall" and prompt.dict["name"] == "print":
                    # super().error(intbase.ErrorType.TYPE_ERROR,
                    #               """Argument to function 'inputi' cannot be of
                    #               type void""")

                    # Apparently we can pass print() as an argument to inputi(); is
                    # this the testcase3 I keep missing?
                    super().output(self.eval_fcall(prompt))
                elif prompt.elem_type == "fcall" and prompt.dict["name"] == "inputi":
                    super().output(self.eval_fcall(prompt))
                elif prompt.elem_type == "fcall" and prompt.dict["name"] == "main":
                    # Is this the test case 3 I'm missing for input?
                    super().error(intbase.ErrorType.TYPE_ERROR,
                                  """Cannot call main() from within main()""")
                else:
                    super().error(intbase.ErrorType.NAME_ERROR, "this shouldn't happen")
            elif len(f.dict["args"]) > 1:
                super().error(intbase.ErrorType.NAME_ERROR,
                              "No inputi() function that takes > 1 parameter")
            return int(super().get_input())
        else:
            super().error(intbase.ErrorType.NAME_ERROR,
                          "Function '" + f.dict["name"] + "' is not defined")

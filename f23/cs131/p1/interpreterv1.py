from brewparse import parse_program
import intbase

class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)

    def run(self, program):
        parsed_program = parse_program(program)
        

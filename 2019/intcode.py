import operator
import sys
import os


class PythonProgram:
    def __init__(self, program):
        self.program = list(map(int, program.split(",")))


# Mode 1 is immediate mode, mode 0 is indirect.
class PythonVm:
    def __init__(self, program, inputs=None):
        self.tape = dict(
            enumerate(
                program.program
                if isinstance(program, Program)
                else map(int, program.split(","))
            )
        )
        self._pc = 0
        self._outputs = []
        self._base = 0
        self._inputs = inputs or []

    def _arg_index(self, arg):
        ix = self._pc + 1 + arg
        mode = self.tape[self._pc] // [100, 1000, 10000][arg] % 10
        if mode == 0:  # Indirect.
            return self.tape.get(ix, 0)
        elif mode == 1:  # Immediate.
            return ix
        elif mode == 2:  # Relative.
            return self._base + self.tape.get(ix, 0)

    def _arg(self, arg):
        return self.tape.get(self._arg_index(arg), 0)

    def _function(self, fn):
        self.tape[self._arg_index(2)] = int(fn(self._arg(0), self._arg(1)))
        self._pc += 4

    def _jump(self, test):
        if test(self._arg(0)):
            self._pc = self._arg(1)
        else:
            self._pc += 3

    def set_tape(self, ix, value):
        self.tape[ix] = value

    def get_tape(self, ix):
        return self.tape[ix]

    def has_output(self):
        return bool(self._outputs)

    def output(self):
        return self._outputs.pop(0)

    def drain_output(self):
        output = self._outputs
        self._outputs = []
        return output

    def input(self, value):
        self._inputs.append(value)

    def complete(self):
        assert self.run()
        return self.drain_output()

    def run(self):
        while self.tape[self._pc] != 99:
            inst = self.tape[self._pc]
            op = inst % 100

            # Math operators.
            if op == 1:
                self._function(operator.add)
            elif op == 2:
                self._function(operator.mul)
            elif op == 7:
                self._function(operator.lt)
            elif op == 8:
                self._function(operator.eq)

            # jumps.
            elif op == 5:
                self._jump(operator.truth)
            elif op == 6:
                self._jump(operator.not_)

            # I/O.
            elif op == 3:
                if not self._inputs:
                    # Need more input.
                    return False
                self.tape[self._arg_index(0)] = self._inputs.pop(0)
                self._pc += 2
            elif op == 4:
                self._outputs.append(self._arg(0))
                self._pc += 2

            # Relative base
            elif op == 9:
                self._base += self._arg(0)
                self._pc += 2

            else:
                assert op == 99
        return True


import icore

py_incode = int(os.getenv("PY_INTCODE") or 0)
Vm = PythonVm if py_incode else icore.Vm
Program = PythonProgram if py_incode else icore.Program

if __name__ == "__main__":
    print(" ".join(map(str, Vm(sys.argv[1], []).complete())))

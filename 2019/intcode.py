import operator

# Mode 1 is immediate mode, mode 0 is indirect.
class Vm:
    def __init__(self, program, inputs=None):
        self.tape = dict(enumerate(map(int, program.split(","))))
        self.pc = 0
        self.outputs = []
        self.relative_base = 0
        self.inputs = inputs or []

    def arg_index(self, arg):
        ix = self.pc + 1 + arg
        mode = self.tape[self.pc] // [100, 1000, 10000][arg] % 10
        if mode == 0:  # Indirect.
            return self.tape.get(ix, 0)
        elif mode == 1:  # Immediate.
            return ix
        elif mode == 2:  # Relative.
            return self.relative_base + self.tape.get(ix, 0)

    def arg(self, arg):
        return self.tape.get(self.arg_index(arg), 0)

    def function(self, fn):
        self.tape[self.arg_index(2)] = fn(self.arg(0), self.arg(1))
        self.pc += 4

    def jump(self, test):
        if test(self.arg(0)):
            self.pc = self.arg(1)
        else:
            self.pc += 3

    def complete(self):
        assert self.run()
        return self.outputs

    def run(self):
        while self.tape[self.pc] != 99:
            inst = self.tape[self.pc]
            op = inst % 100

            # Math operators.
            if op == 1:
                self.function(operator.add)
            elif op == 2:
                self.function(operator.mul)
            elif op == 7:
                self.function(operator.lt)
            elif op == 8:
                self.function(operator.eq)

            # Jumps.
            elif op == 5:
                self.jump(operator.truth)
            elif op == 6:
                self.jump(operator.not_)

            # I/O.
            elif op == 3:
                if not self.inputs:
                    # Need more input.
                    return False
                self.tape[self.arg_index(0)] = self.inputs.pop(0)
                self.pc += 2
            elif op == 4:
                self.outputs.append(self.arg(0))
                self.pc += 2

            # Relative base
            elif op == 9:
                self.relative_base += self.arg(0)
                self.pc += 2

            else:
                assert op == 99
        return True

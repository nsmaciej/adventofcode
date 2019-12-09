import operator

# Mode 1 is immediate mode, mode 0 is indirect.
class Vm:
    def __init__(self, program, inputs=None):
        self.tape = dict(enumerate(map(int, program.split(","))))
        self.pc = 0
        self.outputs = []
        self.relative_base = 0
        self.inputs = inputs or []

    def paramater_index(self, arg, mode):
        ix = self.pc + 1 + arg
        if mode == 0:  # Indirect.
            return self[ix]
        elif mode == 1:  # Immediate.
            return ix
        elif mode == 2:  # Relative.
            return self.relative_base + self[ix]

    def __getitem__(self, ix):
        assert ix >= 0
        return self.tape.get(ix, 0)

    def __setitem__(self, ix, value):
        assert ix >= 0
        self.tape[ix] = value

    def paramater(self, arg, mode):
        return self[self.paramater_index(arg, mode)]

    def store(self, arg, mode, value):
        self[self.paramater_index(arg, mode)] = value

    def operand(self, modes, fn):
        result = fn(self.paramater(0, modes[0]), self.paramater(1, modes[1]))
        self.store(2, modes[2], result)
        self.pc += 4

    def complete(self):
        assert self.run()
        return self.outputs

    # Returns True if halted.
    def run(self):
        while self[self.pc] != 99:
            inst = self[self.pc]
            op = inst % 100
            modes = [(inst // 100) % 10, (inst // 1000) % 10, (inst // 10000) % 10]

            # Math operators.
            if op == 1:
                self.operand(modes, operator.add)
            elif op == 2:
                self.operand(modes, operator.mul)
            elif op == 7:
                self.operand(modes, operator.lt)
            elif op == 8:
                self.operand(modes, operator.eq)

            # I/O.
            elif op == 3:
                if not self.inputs:
                    # Need more input.
                    return False
                self.store(0, modes[0], self.inputs.pop(0))
                self.pc += 2
            elif op == 4:
                self.outputs.append(self.paramater(0, modes[0]))
                self.pc += 2

            # Jumps.
            elif op == 5:
                if self.paramater(0, modes[0]):
                    self.pc = self.paramater(1, modes[1])
                else:
                    self.pc += 3
            elif op == 6:
                if not self.paramater(0, modes[0]):
                    self.pc = self.paramater(1, modes[1])
                else:
                    self.pc += 3

            # Relative base
            elif op == 9:
                self.relative_base += self.paramater(0, modes[0])
                self.pc += 2

            else:
                assert op == 99
        return True

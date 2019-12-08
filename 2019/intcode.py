import operator

# Mode 1 is immediate mode, mode 0 is indirect.
class Vm:
    def __init__(self, tape, inputs=None):
        self.tape = tape.copy()
        self.pc = 0
        self.outputs = []
        self.inputs = inputs or []

    def paramater_index(self, arg, mode):
        ix = self.pc + 1 + arg
        return ix if mode == 1 else self.tape[ix]

    def paramater(self, arg, mode):
        return self.tape[self.paramater_index(arg, mode)]

    def store(self, arg, mode, value):
        self.tape[self.paramater_index(arg, mode)] = value

    def operand(self, modes, fn):
        result = fn(self.paramater(0, modes[0]), self.paramater(1, modes[1]))
        self.store(2, modes[2], result)
        self.pc += 4

    # Returns True if halted.
    def run(self):
        while self.tape[self.pc] != 99:
            encoding = str(self.tape[self.pc]).zfill(5)
            op = int(encoding[3:])
            # Important to reverse it, this way the first mode is the first operand.
            modes = list(map(int, encoding[:3]))[::-1]

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
            else:
                assert op == 99
        return True

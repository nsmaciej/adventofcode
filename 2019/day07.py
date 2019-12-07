import itertools
import operator

# Mode 1 is immediate mode, mode 0 is indirect.
class State:
    def __init__(self, tape):
        self.tape = tape.copy()
        self.pc = 0

    def paramater_index(self, arg, mode):
        ix = self.pc + 1 + arg
        return ix if mode == 1 else self.tape[ix]

    def paramater(self, arg, mode):
        return self.tape[self.paramater_index(arg, mode)]

    def store(self, arg, mode, value):
        self.tape[self.paramater_index(arg, mode)] = value

    def operand(self, modes, fn):
        args = [self.paramater(i, m) for i, m in enumerate(modes[:2])]
        self.store(2, modes[2], fn(*args))
        self.pc += 4

    def instruction(self):
        encoding = str(self.tape[self.pc]).zfill(5)
        op = int(encoding[3:])
        # Important to reverse it, this way the first mode is the first operand.
        modes = list(map(int, encoding[:3]))[::-1]
        return op, modes


def simulate(tape, inputs):
    state = State(tape)
    output = None
    while True:
        op, modes = state.instruction()
        # Math operators.
        if op == 1:
            state.operand(modes, operator.add)
        elif op == 2:
            state.operand(modes, operator.mul)
        elif op == 7:
            state.operand(modes, operator.lt)
        elif op == 8:
            state.operand(modes, operator.eq)

        # I/O.
        elif op == 3:
            state.store(0, modes[0], inputs.pop(0))
            state.pc += 2
        elif op == 4:
            output = state.paramater(0, modes[0])
            state.pc += 2

        # Jumps.
        elif op == 5:
            if state.paramater(0, modes[0]):
                state.pc = state.paramater(1, modes[1])
            else:
                state.pc += 3
        elif op == 6:
            if not state.paramater(0, modes[0]):
                state.pc = state.paramater(1, modes[1])
            else:
                state.pc += 3
        else:
            assert op == 99
            break
    return output


tape = list(map(int, open("inputs/day07.txt").read().split(",")))


def a():
    for x in itertools.permutations(range(5), 5):
        a = simulate(tape, [x[0], 0])
        b = simulate(tape, [x[1], a])
        c = simulate(tape, [x[2], b])
        d = simulate(tape, [x[3], c])
        e = simulate(tape, [x[4], d])
        yield e


print(max(a()))
# print(simulate(tape, 1))
# print(simulate(tape, 5))
# open("inputs/day07.txt")

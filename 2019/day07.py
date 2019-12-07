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


def simulate(name, tape, inp):
    state = State(tape)
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
            r = inp.pop(0)
            state.store(0, modes[0], r)
            state.pc += 2
        elif op == 4:
            r = yield state.paramater(0, modes[0])
            inp.append(r)
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


tape = list(map(int, open("inputs/day07.txt").read().split(",")))


def a():
    for x in itertools.permutations(range(5, 10), 5):
        try:
            a = simulate(0, tape, [x[0], 0])
            b = simulate(1, tape, [x[1], next(a)])
            c = simulate(2, tape, [x[2], next(b)])
            d = simulate(3, tape, [x[3], next(c)])
            e = simulate(4, tape, [x[4], next(d)])
            amps = [a, b, c, d, e]
            signal = next(e)
            i = 0
            while True:
                signal = amps[i].send(signal)
                i = (i + 1) % len(amps)
        except StopIteration:
            yield signal


print(max(a()))

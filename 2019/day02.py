from intcode import Vm, Program
from aoc import *


def simulate(n, v):
    computer = Vm(program)
    computer.set_tape(1, n)
    computer.set_tape(2, v)
    computer.complete()
    return computer.get_tape(0)


program = Program(data(2).read())
print(simulate(12, 2))
for n in range(100):
    for v in range(100):
        if simulate(n, v) == 19690720:
            print(100 * n + v)
            break

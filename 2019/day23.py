from aoc import *
from intcode import Vm, Program


def simulate():
    computers = [Vm(program, [i]) for i in range(50)]
    while True:
        idle = True
        nat = None
        for c in computers:
            if not c.has_input():
                c.input(-1)
            c.run()
            if c.has_output():
                otuput = c.drain_output()
                assert len(otuput) % 3 == 0
                for i in range(0, len(otuput), 3):
                    dest, x, y = otuput[i : i + 3]
                    if dest == 255:
                        nat = x, y
                    else:
                        computers[dest].input(x)
                        computers[dest].input(y)
                        idle = False
        if idle and nat and not any(c.has_input() for c in computers):
            x, y = nat
            yield y
            computers[0].input(x)
            computers[0].input(y)


program = Program(data(23).read())
print(next(simulate()))
seen = set()
for y in simulate():
    if y in seen:
        print(y)
        break
    seen.add(y)

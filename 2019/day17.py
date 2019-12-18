from intcode import Vm
from aoc import *

output = "".join(map(chr, Vm(data(17).read(), []).complete()))
hull = {k: v for k, v in make_grid(output.split("\n")).items() if v != "."}
print(sum(p[0] * p[1] for p in hull if all(p in hull for p in around(p))))

moves = {">": (0, 1), "<": (0, -1), "^": (-1, 0), "v": (1, 0)}
pos = next(k for k, v in hull.items() if v in moves)
delta = moves[hull[pos]]
steps = []
while True:
    if add(pos, delta) not in hull:
        for t in [False, True]:
            if add(pos, turn(delta, t)) in hull:
                delta = turn(delta, t)
                steps.append(["L", "R"][t])
                break
        else:
            break
    else:
        if isinstance(steps[-1], int):
            steps[-1] += 1
        else:
            steps.append(1)
        pos = add(pos, delta)
A = "L,6,R,6,L,12"
B = "R,8,L,12,L,12,R,8"
C = "L,12,R,8,L,6,R,8,L,6"
main = ",".join(map(str, steps)).replace(A, "A").replace(B, "B").replace(C, "C")

vm = Vm(data(17).read())
vm.set_tape(0, 2)
vm.input_line(main)
vm.input_line(A)
vm.input_line(B)
vm.input_line(C)
vm.input_line("n")  # Trace path
print(vm.complete().pop())

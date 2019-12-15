from intcode import Vm, Program
from collections import defaultdict

program = Program(open("inputs/day15.txt").read())
known = {}
oxygen = {}
delta = {1: (-1, 0), 2: (1, 0), 3: (0, -1), 4: (0, 1)}


def ad(a, b):
    return a[0] + b[0], a[1] + b[1]


def reduce(steps):
    r = (0, 0)
    for s in steps:
        r = ad(r, delta[s])
    return r


def flood(steps):
    y, x = reduce(steps)
    if (y, x) in known:
        return
    vm = Vm(program, steps.copy())
    vm.run()
    last = vm.drain_output()[-1]
    known[y, x] = last
    if last == 2:
        return steps
    elif last == 0:
        return
    else:
        for i in range(4):
            r = flood(steps + [i + 1])
            if r:
                return r


def flood2(pos, time):
    if pos in oxygen:
        return
    if pos in known and (known[pos] == 1 or known[pos] == 2):
        if pos not in oxygen:
            oxygen[pos] = time
        for s in range(4):
            r = ad(pos, delta[s + 1])
            if r not in oxygen:
                todo.add((r, time + 1))


r = None
for i in range(4):
    r = r or flood([i + 1])
print(len(r))

todo = set()
flood2(reduce(r), 0)
while todo:
    p, t = todo.pop()
    flood2(p, t)
print(max(oxygen.values()))

from intcode import Vm, Program
from collections import defaultdict
from aoc import *

program = Program(data(15).read())
known = {}
oxygen = {}
delta = {1: (-1, 0), 2: (1, 0), 3: (0, -1), 4: (0, 1)}
back = {1: 2, 2: 1, 3: 4, 4: 3}


def search(vm, pos, t):
    found = None
    for i in range(4):
        np = add(pos, delta[i + 1])
        if np in known:
            continue
        vm.input(i + 1)
        vm.run()
        known[np] = vm.output()
        if known[np] == 2:
            found = pos, t + 1
        elif known[np] == 1:
            r = search(vm, add(pos, delta[i + 1]), t + 1)
            # Finding oxygen doesn't mean we are done exploring. We need the entire ship
            # for the second step to work.
            if r:
                found = r
        if known[np] in [1, 2]:
            vm.input(back[i + 1])
            vm.run()
            vm.output()  # Pop
        # Otherwise we found a wall, do not move back.
    return found


def fill(pos, time):
    if pos in oxygen:
        return
    if pos in known and known[pos] in [1, 2]:
        oxygen[pos] = time
        for s in range(4):
            todo.add((add(pos, delta[s + 1]), time + 1))


vm = Vm(program, [])
pos, t = search(vm, (0, 0), 0)
print(t)

todo = {(pos, 1)}
while todo:
    p, t = todo.pop()
    fill(p, t)
print(max(oxygen.values()))

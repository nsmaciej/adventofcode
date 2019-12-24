from intcode import Vm, Program
from collections import deque
from aoc import *


def search(start):
    system = None
    for cmd in range(1, 5):
        pos = add(start, moves[cmd])
        if pos in ship:
            continue
        droid.input(cmd)
        droid.run()
        out = ship[pos] = droid.output()
        if ship[pos] == 2:
            system = pos
        elif ship[pos] == 1:
            if r := search(pos):
                # Do not stop exploring, we need to map the entire ship.
                system = r
        else:
            continue  # Found a wall. Do not move back.
        droid.input(back[cmd])
        droid.run()
        droid.output()  # Pop
    return system


def distances(start):
    queue = deque([start])
    dist = {start: 0}
    while queue:
        pos = queue.pop()
        for p in around(pos):
            if p not in dist and ship.get(p) in [1, 2]:
                dist[p] = dist[pos] + 1
                queue.append(p)
    return dist


ship = {}
moves = {1: (-1, 0), 2: (1, 0), 3: (0, -1), 4: (0, 1)}
back = {1: 2, 2: 1, 3: 4, 4: 3}
droid = Vm(data(15).read())
system = search((0, 0))
print(distances((0, 0))[system])
print(max(distances(system).values()))

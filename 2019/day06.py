from aoc import *


def trace(obj):
    path = [obj]
    while obj in orbits:
        obj = orbits[obj]
        path.append(obj)
    return list(reversed(path))


data = [x.strip().split(")") for x in data(6)]
orbits = {y: x for x, y in data}
you, san = trace("YOU"), trace("SAN")
offset = sum(x == y for x, y in zip(you, san))
print(sum(len(trace(x)) - 1 for x in orbits.keys()))
print(len(you) - you.index(you[offset + 1]) + len(san) - san.index(san[offset + 1]))

from aoc import *


def trace(obj):
    path = {obj}
    while obj in orbits:
        obj = orbits[obj]
        path.add(obj)
    return path


orbits = {y: x for x, y in (x.strip().split(")") for x in data(6))}
print(sum(len(trace(x)) - 1 for x in orbits.keys()))
print(len(trace("YOU") ^ trace("SAN")) - 2)

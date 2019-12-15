from aoc import *


def points(turns):
    times = {}
    pos = (0, 0)
    t = 0
    for turn in turns.split(","):
        for _ in range(int(turn[1:])):
            pos = add(pos, DIRECTIONS[turn[0]])
            t += 1
            times[pos] = t
    return times


DIRECTIONS = {"U": (0, 1), "D": (0, -1), "L": (-1, 0), "R": (1, 0)}
a, b = map(points, data(3))
intersections = set(a.keys()) & set(b.keys())
print(min(map(lambda x: abs(x[0]) + abs(x[1]), intersections)))
print(min(map(lambda x: a[x] + b[x], intersections)))

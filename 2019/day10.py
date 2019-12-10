import numpy as np
from math import gcd, atan2, pi
from collections import defaultdict


def asteroid_angles(coord):
    angles = defaultdict(list)
    for other, i in np.ndenumerate(field):
        if other != coord and i == "#":
            dy, dx = other[0] - coord[0], other[1] - coord[1]
            d = gcd(dy, dx)
            angles[dy // d, dx // d].append(other)
    return angles


def angle(x):
    return (atan2(x[0], x[1]) + pi / 2) % (2 * pi)


field = [list(x.strip()) for x in open("inputs/day10.txt")]
asteroids = {i: asteroid_angles(i) for i, v in np.ndenumerate(field) if v == "#"}
targets = max(asteroids.values(), key=len)
print(len(targets))
win_y, win_x = targets[sorted(targets.keys(), key=angle)[199]][0]
print(win_x * 100 + win_y)

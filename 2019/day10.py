import numpy as np
from math import gcd, atan2, pi, hypot
from collections import defaultdict


def delta(a, b):
    return a[0] - b[0], a[1] - b[1]


def asteroid_angles(candidate):
    angles = defaultdict(list)
    for coord, kind in np.ndenumerate(field):
        if coord != candidate and kind == "#":
            # Careful, the order here matters.
            dy, dx = delta(coord, candidate)
            d = gcd(dy, dx)
            angles[dy // d, dx // d].append(coord)
    return angles


def angle(x):
    return (atan2(x[0], x[1]) + pi / 2) % (2 * pi)


field = [list(x.strip()) for x in open("inputs/day10.txt")]
asteroids = {i: asteroid_angles(i) for i, v in np.ndenumerate(field) if v == "#"}
station = max(asteroids, key=lambda x: len(asteroids[x]))
targets = asteroids[station]
print(len(targets))
win_angle = targets[sorted(targets.keys(), key=angle)[199]]
win_y, win_x = sorted(win_angle, key=lambda x: hypot(*delta(x, station)))[0]
print(win_x * 100 + win_y)

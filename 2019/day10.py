import numpy as np
from math import gcd, atan2, pi
from collections import defaultdict
from functools import reduce


def detects(coord):
    coverage = defaultdict(list)
    for other, i in np.ndenumerate(field):
        if other != coord and i == "#":
            dy, dx = other[0] - coord[0], other[1] - coord[1]
            d = gcd(dy, dx)
            coverage[dy // d, dx // d].append(other)
    return coverage


def angle(x):
    return (atan2(x[0], x[1]) + pi / 2) % (2 * pi)


field = np.array([list(x.strip()) for x in open("inputs/day10.txt")])
coverage = {i: detects(i) for i, v in np.ndenumerate(field) if v == "#"}
station = max(coverage, key=lambda x: len(coverage[x]))
targets = coverage[station]
print(len(targets))

angles = sorted(targets.keys(), key=angle)
current_angle = 0
for i in range(200):
    while not targets[angles[current_angle]]:
        current_angle = (current_angle + 1) % len(targets)
    target = targets[angles[current_angle]].pop()
    current_angle += 1
print(target[1] * 100 + target[0])

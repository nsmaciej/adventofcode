from math import gcd, atan2, pi, hypot
from aoc import *


def asteroid_angles(candidate):
    angles = defaultdict(list)
    for coord, kind in field.items():
        if coord != candidate and kind == "#":
            dy, dx = sub(coord, candidate)  # Careful, the order here matters.
            d = gcd(dy, dx)
            angles[dx // d, -dy // d].append(coord)  # Note the rotated coordinates.
    return angles


def angle(x):
    return atan2(x[0], x[1]) % (2 * pi)


field = make_grid(data(10).read().splitlines())
asteroids = {i: asteroid_angles(i) for i, v in field.items() if v == "#"}
station = max(asteroids, key=lambda x: len(asteroids[x]))
targets = asteroids[station]
print(len(targets))

win_angle = targets[sorted(targets.keys(), key=angle)[199]]
win_y, win_x = min(win_angle, key=lambda x: hypot(*sub(x, station)))
print(win_x * 100 + win_y)

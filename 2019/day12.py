from aoc import *
import re
from itertools import count
from math import gcd


def cmp(x, y):
    return (x > y) - (x < y)


def lcm(a, b):
    return a * b // gcd(a, b)


def part_one():
    pos = [x.copy() for x in moons]
    vel = [[0, 0, 0] for _ in range(len(pos))]
    for _ in range(1000):
        for d in range(3):
            for m in range(len(pos)):
                for k in range(m):
                    r = cmp(pos[m][d], pos[k][d])
                    vel[m][d] -= r
                    vel[k][d] += r
            for m in range(len(pos)):
                pos[m][d] += vel[m][d]
    return sum(sum(map(abs, vel[m])) * sum(map(abs, pos[m])) for m in range(len(pos)))


def cycle(d):
    pos = [x[d] for x in moons]
    vel = [0] * len(pos)
    for age in count(1):
        for m in range(len(pos)):
            for k in range(m):
                r = cmp(pos[m], pos[k])
                vel[m] -= r
                vel[k] += r
        if not any(vel):
            return 2 * age  # TODO: Provide an explanation.
        for m in range(len(pos)):
            pos[m] += vel[m]


def parse(x):
    return list(map(int, re.match(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", x).groups()))


moons = list(map(parse, data(12)))
print(part_one())
print(lcm(lcm(cycle(0), cycle(1)), cycle(2)))

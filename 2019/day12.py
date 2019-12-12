import re
import numpy as np


def parse(x):
    return list(map(int, re.match(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", x).groups()))


def cmp(x, y):
    return (x > y).astype(int) - (x < y).astype(int)


moons = np.array(list(map(parse, open("inputs/day12.txt"))))
velocity = np.zeros_like(moons)
for _ in range(1000):
    new_velocity = np.copy(velocity)
    for moon in range(len(moons)):
        for dim in range(3):
            new_velocity[moon, dim] += cmp(moons[:, dim], moons[moon, dim]).sum()
    velocity = new_velocity
    moons += new_velocity

print(sum(abs(moons).sum(1) * abs(velocity).sum(1)))

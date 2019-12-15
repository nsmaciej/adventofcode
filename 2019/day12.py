import re
import numpy as np
from aoc import *


def parse(x):
    return list(map(int, re.match(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", x).groups()))


class Universe:
    def __init__(self, moons):
        self.moons = np.copy(moons)
        self.vel = np.zeros_like(moons)
        self.age = 0

    def step(self):
        for i in range(len(self.moons)):
            x, y = self.moons, self.moons[i, :]
            self.vel[i] += ((x > y).astype(int) - (x < y).astype(int)).sum(0)
        self.moons += self.vel
        self.age += 1

    def energy(self):
        return sum(abs(self.moons).sum(1) * abs(self.vel).sum(1))


moons = np.array(list(map(parse, data(12))))
universe = Universe(moons)
for _ in range(1000):
    universe.step()
print(universe.energy())

universe = Universe(moons)
cycle = {}
while len(cycle) < 3:
    universe.step()
    for i in range(3):
        if not universe.vel[:, i].any() and i not in cycle:
            cycle[i] = 2 * universe.age
print(np.lcm.reduce(list(cycle.values())))

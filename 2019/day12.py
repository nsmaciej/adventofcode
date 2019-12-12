import re
import numpy as np


def parse(x):
    return list(map(int, re.match(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", x).groups()))


def moon_velocity(x, y):
    return ((x > y).astype(int) - (x < y).astype(int)).sum(0)


class Universe:
    def __init__(self, moons):
        self.moons = np.copy(moons)
        self.vel = np.zeros_like(moons)

    def step(self):
        for i in range(len(self.moons)):
            self.vel[i] += moon_velocity(self.moons, self.moons[i, :])
        self.moons += self.vel

    def energy(self):
        return sum(abs(self.moons).sum(1) * abs(self.vel).sum(1))


moons = np.array(list(map(parse, open("inputs/day12.txt"))))
universe = Universe(moons)
for _ in range(1000):
    universe.step()
print(universe.energy())

universe = Universe(moons)
seen = {axis: {} for axis in range(3)}
cycle = {}
age = 0
while len(cycle) < 3:
    universe.step()
    for i in range(3):
        key = (tuple(universe.moons[:, i]), tuple(universe.vel[:, i]))
        if key in seen[i]:
            cycle[i] = age - seen[i][key]
        else:
            seen[i][key] = age
    age += 1
print(np.lcm.reduce(list(cycle.values())))

import time
import re
import numpy as np
import sys


def parse(x):
    return re.findall(r"-?\d+", x)


def show(x):
    indices = x - x.min(0)
    r = np.full((indices[:, 1].max() + 1, indices[:, 0].max() + 1), " ")
    r[indices[:, 1], indices[:, 0]] = "."
    print("\n".join("".join(x) for x in r))


points = np.array(list(map(parse, open("inputs/day10.txt"))), int)
position = points[:, 0:2]
step = 0
while position[:, 1].max() - position[:, 1].min() != 9:
    position += points[:, 2:4]
    step += 1
show(position)
print(step)

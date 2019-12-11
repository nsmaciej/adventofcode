from intcode import Vm
from operator import itemgetter
import numpy as np


def run(start):
    hull = {}
    robot = Vm(open("inputs/day11.txt").read(), [start])
    x, y = 0, 0
    dx, dy = 0, -1
    while not robot.run():
        hull[x, y] = robot.outputs.pop(0)
        if robot.outputs.pop(0) == 0:
            dx, dy = dy, -dx
        else:
            dx, dy = -dy, dx
        x += dx
        y += dy
        robot.inputs.append(hull.get((x, y), 0))
    return hull


print(len(run(0)))

hull = run(1)
sy, _ = max(hull.keys(), key=itemgetter(0))
_, sx = max(hull.keys(), key=itemgetter(1))
out = np.zeros((sy + 1, sx + 1), int)
out[0, 0] = 1
for pos, color in hull.items():
    out[pos] = color
print("\n".join("".join(map(str, row)).replace("0", " ") for row in out.T))

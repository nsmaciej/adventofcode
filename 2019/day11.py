from intcode import Vm
import numpy as np


def run(start):
    hull = {}
    robot = Vm(program, [start])
    x, y = 0, 0
    dx, dy = 0, -1
    while not robot.run():
        hull[y, x] = robot.output()
        if robot.output() == 0:
            dx, dy = dy, -dx
        else:
            dx, dy = -dy, dx
        x += dx
        y += dy
        robot.input(hull.get((y, x), 0))
    return hull


program = open("inputs/day11.txt").read()
print(len(run(0)))
hull = np.array([p for p, c in run(1).items() if c == 1])
hull += hull.min(0)
out = np.zeros(hull.max(0) + 1, int)
out[tuple(hull.T)] = 1
print("\n".join("".join(map(str, row)).replace("0", " ") for row in out))

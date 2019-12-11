from intcode import Vm
import numpy as np


def run(start):
    hull = {}
    robot = Vm(program, [start])
    pos = np.array([0, 0])
    delta = np.array([-1, 0])
    while not robot.run():
        hull[tuple(pos)] = robot.output()
        turn = [[0, -1], [1, 0]] if robot.output() else [[0, 1], [-1, 0]]
        delta = delta @ turn
        pos += delta
        robot.input(hull.get(tuple(pos), 0))
    return hull


program = open("inputs/day11.txt").read()
print(len(run(0)))
hull = np.array([p for p, c in run(1).items() if c == 1])
hull += hull.min(0)
out = np.zeros(hull.max(0) + 1, int)
out[tuple(hull.T)] = 1
print("\n".join("".join(map(str, row)).replace("0", " ") for row in out))

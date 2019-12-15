from intcode import Vm
from aoc import *


def run(start):
    hull = {}
    robot = Vm(program, [start])
    pos = (0, 0)
    dx, dy = 0, -1
    while not robot.run():
        hull[pos] = robot.output()
        if robot.output() == 0:
            dx, dy = dy, -dx
        else:
            dx, dy = -dy, dx
        pos = add(pos, (dy, dx))
        robot.input(hull.get(pos, 0))
    return hull


program = data(11).read()
print(len(run(0)))
print_grid(run(1), mapping={0: " ", 1: "#"})

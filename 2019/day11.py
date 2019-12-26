from intcode import Vm
from aoc import *


def run(start):
    hull = {}
    robot = Vm(program, [start])
    pos = (0, 0)
    delta = (-1, 0)
    while not robot.run():
        hull[pos] = robot.output()
        delta = turn(delta, robot.output())
        pos = add(pos, delta)
        robot.input(hull.get(pos, 0))
    return hull


program = data(11).read()
print(len(run(0)))
print(ocr(run(1)))

from aoc import *
from intcode import Vm, Program


def pulled(x, y):
    return bool(Vm(drone, [x, y]).complete()[-1])


drone = Program(data(19).read())
print(sum(pulled(x, y) for x in range(50) for y in range(50)))

size = 99
bx, by = 0, size
while not pulled(bx, by):
    bx += 1
# Keep lowering the square until both its lower-left and upper-right corner are pulled.
while not pulled(bx, by) or not pulled(bx + size, by - size):
    by += 1
    while not pulled(bx, by):
        bx += 1
print(bx * 10000 + (by - size))

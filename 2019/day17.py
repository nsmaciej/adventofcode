from intcode import Vm
from aoc import *

robot = Vm(data(17).read(), [])
output = "".join(list(map(chr, robot.complete())))
hull = {
    (y, x): val
    for y, row in enumerate(output.split("\n"))
    for x, val in enumerate(row)
    if val != "."
}
h = max(x[0] for x in hull)
w = max(x[1] for x in hull)
r = 0
for y in range(h):
    for x in range(w):
        if (
            (y, x) in hull
            and (y - 1, x) in hull
            and (y + 1, x) in hull
            and (y, x - 1) in hull
            and (y, x + 1) in hull
        ):
            r += y * x

print(r)

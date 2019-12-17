from intcode import Vm
from aoc import *

output = "".join(map(chr, Vm(data(17).read(), []).complete()))
hull = {
    (y, x): val
    for y, row in enumerate(output.split("\n"))
    for x, val in enumerate(row)
    if val != "."
}
print(sum(p[0] * p[1] for p in hull if all(p in hull for p in around(p))))

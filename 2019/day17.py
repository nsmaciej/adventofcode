from intcode import Vm
from aoc import *
from itertools import product

output = "".join(map(chr, Vm(data(17).read(), []).complete()))
hull = {k: v for k, v in make_grid(output.split("\n")).items() if v != "."}
print(sum(p[0] * p[1] for p in hull if all(p in hull for p in around(p))))


def find_path():
    pos = next(k for k, v in hull.items() if v in moves)
    delta = moves[hull[pos]]
    path = []
    assert add(pos, delta) not in hull  # Assume we have to turn.
    while True:
        direction = add(pos, turn(delta, True)) in hull
        delta = turn(delta, direction)
        steps = 0
        while add(pos, delta) in hull:
            steps += 1
            pos = add(pos, delta)
        if not steps:
            break
        yield ["L", "R"][direction]
        yield str(steps)


def serialize(path):
    return ",".join(path)


def find_mid(l, m, r):
    def short(part):
        return sum(map(len, part)) + len(part) - 1 <= 20

    def check(part):
        return all(x == y for x, y in zip(path[i:], part))

    ls, ms, rs = path[:l], None, path[-r:]
    if not short(ls) or not short(rs):
        return False
    i = 0
    while i < len(path):
        if check(ls):
            i += l
        elif check(rs):
            i += r
        elif ms and check(ms):
            i += m
        elif not ms and short(path[i : i + m]):
            ms = path[i : i + m]
            i += m
        else:
            return False
    return map(serialize, (ls, ms, rs))


moves = {">": (0, 1), "<": (0, -1), "^": (-1, 0), "v": (1, 0)}
path = list(find_path())
a, b, c = first(find_mid(*x) for x in product(range(4, 11), repeat=3))
main = serialize(path).replace(a, "A").replace(b, "B").replace(c, "C")
vm = Vm(data(17).read())
vm.set_tape(0, 2)
for line in [main, a, b, c, "n"]:
    vm.input_line(line)
print(vm.complete().pop())

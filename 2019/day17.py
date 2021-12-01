from intcode import Vm
from aoc import *
from itertools import product

output = "".join(map(chr, Vm(data(17).read()).complete()))
hull = {k: v for k, v in make_grid(output.split("\n")).items() if v != "."}
print(sum(p[0] * p[1] for p in hull if all(p in hull for p in around(p))))


def find_path():
    pos = next(k for k, v in hull.items() if v in moves)
    delta = moves[hull[pos]]
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


def find_main(lengths):
    routines = [path[: lengths[0]]]
    i = 0
    while i < len(path):
        for r in routines:
            if all(x == y for x, y in zip(r, path[i:])):
                i += len(r)
                break
        else:
            if len(routines) == len(lengths):
                return
            candidate = path[i : i + lengths[len(routines)]]
            if len(serialize(candidate)) > 20:
                return
            routines.append(candidate)
            i += len(candidate)
    return map(serialize, routines)


moves = {">": (0, 1), "<": (0, -1), "^": (-1, 0), "v": (1, 0)}
path = list(find_path())
a, b, c = first(map(find_main, product(range(4, 11), repeat=3)))
main = serialize(path).replace(a, "A").replace(b, "B").replace(c, "C")
vacuum = Vm(data(17).read())
vacuum.set_tape(0, 2)
for line in [main, a, b, c, "n"]:
    vacuum.input_line(line)
print(vacuum.complete()[-1])

from aoc import *
import re

cmds = list(data(22).read().splitlines())
N = 10007


def at_index(i):
    for cmd in reversed(cmds):
        if cmd == "deal into new stack":
            i = N - 1 - i

        elif m := re.match("cut (-?[0-9]+)", cmd):
            val = int(m[1])
            i = (val + i) % N

        elif m := re.match("deal with increment ([0-9]+)", cmd):
            val = int(m[1])
            i = (i * pow(val, -1, N)) % N
    return i


print(next(x for x in range(N) if at_index(x) == 2019))

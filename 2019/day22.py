from aoc import *
import re

cmds = list(data(22).read().splitlines())
N = 10007


def linear_fn():
    a, b = 1, 0
    for cmd in reversed(cmds):
        if cmd == "deal into new stack":
            a *= -1
            b = N - 1 - b

        elif m := re.match("cut (-?[0-9]+)", cmd):
            b += int(m[1])

        elif m := re.match("deal with increment ([0-9]+)", cmd):
            m = pow(int(m[1]), -1, N)
            a *= m
            b *= m
        a %= N
        b %= N
    return a, b


a, b = linear_fn()
print(next(i for i in range(N) if (a * i + b) % N == 2019))
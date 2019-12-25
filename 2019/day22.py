from aoc import *
import re


def apply(a, b, k):
    # Sadly had to take an intense hint for this.
    if k == 1:
        return a, b
    elif k % 2 == 0:
        return apply((a * a) % n, (a * b + b) % n, k // 2)
    else:
        c, d = apply(a, b, k - 1)
        return (a * c) % n, (a * d + b) % n


def linear_fn():
    a, b = 1, 0
    for cmd in reversed(data(22).read().splitlines()):
        if cmd == "deal into new stack":
            a *= -1
            b = n - 1 - b
        elif m := re.match("cut (-?[0-9]+)", cmd):
            b += int(m[1])
        elif m := re.match("deal with increment ([0-9]+)", cmd):
            m = pow(int(m[1]), -1, n)
            a *= m
            b *= m
        a %= n
        b %= n
    return a, b


n = 10007
a, b = linear_fn()
print(next(i for i in range(n) if (a * i + b) % n == 2019))

n = 119315717514047
k = 101741582076661
a, b = apply(*linear_fn(), k)
print((a * 2020 + b) % n)

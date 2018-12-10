import itertools
import collections


def diff(a, b):
    r = 0
    for i, j in zip(a, b):
        if i != j:
            r += 1
    return r


data = [x.strip() for x in open("b.txt").readlines()]
for x in data:
    for y in data:
        if diff(x, y) == 1:
            print(x)

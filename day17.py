import re
import numpy as np
import fileinput
from heapq import *


def show(scan):
    colours = {
        "#": "\x1b[31m#\x1b[0m",
        "~": "\x1b[36m~\x1b[0m",
        "|": "\x1b[36m|\x1b[0m",
    }
    print("\n" + "\n".join("".join(colours.get(y, y) for y in x) for x in scan))


def parse(line):
    m = re.match(r"([xy])=(\d+), [xy]=(\d+)..(\d+)", line)
    first = int(m[2]), int(m[2]) + 1
    second = int(m[3]), int(m[4]) + 1
    if m[1] == "x":
        return first, second
    return second, first


def below(p):
    return p[0] + 1, p[1]


def above(p):
    return p[0] - 1, p[1]


def right(p):
    return p[0], p[1] + 1


def left(p):
    return p[0], p[1] - 1


def valid(p):
    return 0 <= p[0] < scan.shape[0] and 0 <= p[1] < scan.shape[1]


def flow(p, move):
    while scan[move(p)] in ".|" and scan[below(move(p))] in "~#":
        p = move(p)
        scan[p] = "|"
    return p


def drip(p, move):
    if scan[below(r)] == "#" and scan[move(r)] == "." and scan[below(move(r))] == ".":
        pour(move(l))


def pour(p, go_up=True):
    scan[p] = "|"
    while scan[below(p)] == ".":
        p = below(p)
        scan[p] = "|"
    l = flow(p, left)
    r = flow(p, right)
    if scan[left(l)] == "#" and scan[right(r)] == "#":
        scan[l[0], l[1] : r[1] + 1] = "~"
    drip(l, left)
    drip(r, right)
    while go_up and valid(above(p)) and scan[above(p)] == "|":
        p = above(p)
        pour(p, go_up=False)


fd = fileinput.input()
xs, ys = np.array(list(map(parse, fd))).transpose((1, 0, 2))
source_x = 500 - xs.min() + 1
xs -= xs.min() - 1
ys -= ys.min()
scan = np.full((ys.max(), xs.max() + 1), ".")
for (y0, y1), (x0, x1) in zip(ys, xs):
    scan[y0:y1, x0:x1] = "#"

pour((0, source_x))

standing = (scan == "~").sum()
print(standing + (scan == "|").sum())
print(standing)

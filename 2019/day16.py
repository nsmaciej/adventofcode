from aoc import *

phase = list(map(int, data(16).read().strip()))
# phase = [1, 2, 3, 4, 5, 6, 7, 8]


def run(phase, offset):
    phase = phase[offset : offset + 8]
    for p in range(100):
        print(p)
        print("".join(map(str, phase)))
        phase = [apply(phase, offset, i) for i in range(len(phase))]
    print("".join(map(str, phase)))
    return "".join(map(str, phase))


def apply(phase, offset, ix):
    pat = [0, 1, 0, -1]
    pi, pl = 0, ix
    r = 0
    for i in range(len(phase)):
        if pl == 0:
            pi += 1
            pl = ix + 1
        if pi == 4:
            pi = 0
        r += phase[i] * pat[pi]
        pl -= 1
    return abs(r) % 10


print(run(phase, 0))

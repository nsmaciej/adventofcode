from aoc import *



def run(phase):
    for p in range(100):
        phase = [digit(phase, i) for i in range(len(phase))]
    return "".join(map(str, phase[:8]))


def digit(phase, ix):
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


phase = list(map(int, data(16).read().strip()))
print(run(phase))
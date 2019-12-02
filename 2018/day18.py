import numpy as np


def at(m, y, x):
    if 0 <= y < m.shape[0] and 0 <= x < m.shape[1]:
        return m[y, x]
    return "."


def decide(m, ix):
    counts = {".": 0, "#": 0, "|": 0}
    for y, x in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]:
        counts[at(m, ix[0] + y, ix[1] + x)] += 1
    if m[ix] == ".":
        return "|" if counts["|"] >= 3 else "."
    if m[ix] == "#":
        return "#" if counts["#"] >= 1 and counts["|"] >= 1 else "."
    if m[ix] == "|":
        return "#" if counts["#"] >= 3 else "|"


def advance(m):
    r = m.copy()
    for ix in np.ndindex(m.shape):
        r[ix] = decide(m, ix)
    return r


def score(m):
    return (m == "#").sum() * (m == "|").sum()


def simulate(area, goal):
    seen = {}
    i = 0
    skipped = False
    while i < goal:
        old_score = score(area)
        area = advance(area)
        new_score = score(area)
        key = new_score, new_score - old_score
        if key in seen:
            old_i = i
            period = i - seen[key]
            i += 1 + ((goal - i) // period) * period
            break
        seen[key] = i
        i += 1
    while i < goal:
        area = advance(area)
        i += 1
    return score(area)


area = np.array([list(x.strip()) for x in open("inputs/day18.txt")])
print(simulate(area, 10))
print(simulate(area, 1_000_000_000))

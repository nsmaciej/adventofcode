from aoc import *


def neighbours(p):
    return sum(grid[p] if p in grid else False for p in around(p))


def state(p):
    if grid[p]:
        return neighbours(p) == 1
    return 0 < neighbours(p) < 3


grid = {i: v == "#" for i, v, in make_grid(data(24).read().splitlines()).items()}
seen = {tuple(grid.values())}
while True:
    grid = {i: state(i) for i in grid}
    key = tuple(grid.values())
    if key in seen:
        print(sum(v * 1 << i for i, v in enumerate(grid.values())))
        break
    seen.add(key)

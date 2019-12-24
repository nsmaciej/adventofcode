# TODO: Rewrite, maybe use an adjacency list. Two parts in one.
from aoc import *


def neighbours(level, p):
    count = 0
    for m in around():
        x = add(p, m)
        if x == (2, 2):
            # Invert direction and move towards the edge of the inner grid.
            ny, nx = 2 - m[0] * 2, 2 - m[1] * 2
            if level + 1 in grid:
                for i in range(5):
                    count += grid[level + 1][(1 * i, nx) if m[0] == 0 else (ny, 1 * i)]
        elif x not in grid[level]:
            # We are not in grid but aren't (2, 2). Follow this direction out from
            # the middle of the outer grid.
            if level - 1 in grid:
                count += grid[level - 1][add((2, 2), m)]
        else:
            count += grid[level][x]
    return count


def state(level, p):
    n = neighbours(level, p)
    return n == 1 if grid[level][p] else 0 < n < 3


grid = {
    i: v == "#"
    for i, v, in make_grid(data(24).read().splitlines()).items()
    if i != (2, 2)
}
grid = {0: grid}
for turn in range(200):
    if sum(grid[min(grid)].values()) > 0:
        grid[min(grid) - 1] = {i: False for i in grid[0]}
    if sum(grid[max(grid)].values()) > 0:
        grid[max(grid) + 1] = {i: False for i in grid[0]}
    grid = {level: {i: state(level, i) for i in grid[level]} for level in grid}
print(sum(sum(g.values()) for g in grid.values()))

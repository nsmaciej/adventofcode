from aoc import *


def adj_list(grid):
    adj = defaultdict(list)
    for i in grid:
        for k in around(i):
            if k in grid:
                adj[i].append((0, *k))
    return adj


def tick(adj, alive):
    neighbours = defaultdict(int)
    next_alive = set()
    for p in alive:
        for a in adj[p[1:]]:
            delta, y, x = a
            neighbours[p[0] + delta, y, x] += 1
    for p in alive:
        if neighbours[p] == 1:
            next_alive.add(p)
    for p, c in neighbours.items():
        if 0 < c < 3 and p not in alive:
            next_alive.add(p)
    return next_alive


def part_one():
    adj = adj_list(grid)
    alive = first_alive
    seen = set()
    while frozenset(alive) not in seen:
        seen.add(frozenset(alive))
        alive = tick(adj, alive)
    return sum(1 << i for i, v in enumerate(grid) if (0, *v) in alive)


def part_two():
    adj = adj_list({i: v for i, v in grid.items() if i != (2, 2)})
    for x in range(5):
        adj[0, x].append((1, 1, 2))
        adj[x, 4].append((1, 2, 3))
        adj[4, x].append((1, 3, 2))
        adj[x, 0].append((1, 2, 1))
        adj[1, 2].append((-1, 0, x))
        adj[2, 3].append((-1, x, 4))
        adj[3, 2].append((-1, 4, x))
        adj[2, 1].append((-1, x, 0))

    alive = first_alive
    for _ in range(200):
        alive = tick(adj, alive)
    return len(alive)


grid = {i: v == "#" for i, v, in make_grid(data(24).read().splitlines()).items()}
first_alive = {(0, *i) for i, v in grid.items() if v}
print(part_one())
print(part_two())

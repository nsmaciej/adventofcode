from aoc import *
from collections import deque


def distance(start):
    queue = deque([start])
    dist = {}
    dist[start] = 0
    while queue:
        pos = queue.popleft()
        for p in around(pos):
            if p in grid and grid[p] != "#" and not grid[p].isupper() and p not in dist:
                dist[p] = dist[pos] + 1
                queue.append(p)
    return {k: dist[key_pos[k]] for k in keys if key_pos[k] in dist}


def solve():
    def inner(pos, got, td):
        nonlocal shortest, dp
        dp_key = (pos, got)
        if td >= shortest or td >= dp.get(dp_key, inf):
            return
        if len(got) == len(keys):
            if td < shortest:
                shortest = td
            return
        dp[dp_key] = td
        key_dist = distance(pos)
        for key in keys - got:
            if key in key_dist:
                if key in door_pos:
                    grid[door_pos[key]] = "."
                inner(key_pos[key], got | {key}, td + key_dist[key])
                if key in door_pos:
                    grid[door_pos[key]] = key.upper()

    dp = {}
    shortest = inf
    inner(you, frozenset(), 0)
    return shortest


grid = make_grid(data(18).read().splitlines())
you = next(k for k, v in grid.items() if v == "@")
grid[you] = "."

# Compute some useful data.
keys = {v for k, v in grid.items() if v.islower()}
key_pos = {v: k for k, v in grid.items() if v.islower()}
door_pos = {v.lower(): k for k, v in grid.items() if v.isupper()}
print(solve())

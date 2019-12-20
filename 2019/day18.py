from aoc import *
from collections import deque


def distance(start):
    queue = deque([start])
    dist, needed = {}, {}
    dist[start] = 0
    needed[start] = set()
    while queue:
        pos = queue.popleft()
        for p in around(pos):
            if p in grid and grid[p] != "#" and p not in dist:
                val = grid[p]
                dist[p] = dist[pos] + 1
                needed[p] = needed[pos] | ({val.lower()} if val.isupper() else set())
                queue.append(p)
    return {
        k: (dist[key_pos[k]], needed[key_pos[k]]) for k in keys if key_pos[k] in dist
    }


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
        for key in keys - got:
            if dist[pos][key][1] <= got:
                if key in door_pos:
                    grid[door_pos[key]] = "."
                inner(key_pos[key], got | {key}, td + dist[pos][key][0])
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
key_ix = {k: i for i, k in enumerate(keys)}
door_pos = {v.lower(): k for k, v in grid.items() if v.isupper()}
dist = {you: distance(you)}
for k in keys:
    dist[key_pos[k]] = distance(key_pos[k])
print(solve())

from aoc import *
from collections import deque


def distance(start, end):
    queue = deque([start])
    dist = {}
    dist[start] = 0
    while queue:
        pos = queue.popleft()
        for p in around(pos):
            if p in grid and grid[p] != "#" and not grid[p].isupper() and p not in dist:
                dist[p] = dist[pos] + 1
                queue.append(p)
    return dist.get(end)


def solve():
    def inner(pos, got, td):
        nonlocal shortest, dp
        if td >= shortest:
            return
        dp_key = (pos, frozenset(got))
        if td >= dp.get(dp_key, 1_000_000):
            return
        if len(got) == 2:
            print(pos, got)
        dp[dp_key] = td

        if len(got) == len(keys):
            if td < shortest:
                shortest = td

        for key in keys - set(got):
            dist = distance(pos, key_pos[key])
            if dist:
                if key in door_pos:
                    grid[door_pos[key]] = "."
                inner(key_pos[key], got + [key], td + dist)
                if key in door_pos:
                    grid[door_pos[key]] = key.upper()

    dp = {}
    shortest = 99999
    inner(you, [], 0)
    return shortest


grid = make_grid(data(18).read().splitlines())
you = next(k for k, v in grid.items() if v == "@")
grid[you] = "."

# Compute some useful data.
keys = {v for k, v in grid.items() if v.islower()}
key_pos = {v: k for k, v in grid.items() if v.islower()}
door_pos = {v.lower(): k for k, v in grid.items() if v.isupper()}
print(solve())

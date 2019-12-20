from aoc import *
from collections import deque


class Solver:
    def __init__(self, text):
        self.grid = make_grid(text.splitlines())
        self.bots = [i for i, v in self.grid.items() if v == "@"]
        # This shouldn't matter, but let's just leave it in for now.
        for b in self.bots:
            self.grid[b] = "."

        # Compute some useful data.
        self.keys = {v for k, v in self.grid.items() if v.islower()}
        self.key_pos = {v: k for k, v in self.grid.items() if v.islower()}
        self.key_ix = {k: i for i, k in enumerate(self.keys)}
        self.door_pos = {v.lower(): k for k, v in self.grid.items() if v.isupper()}
        self.dist = {self.bots[0]: self.distance(self.bots[0])}
        for k in self.keys:
            self.dist[self.key_pos[k]] = self.distance(self.key_pos[k])

    def distance(self, start):
        queue = deque([start])
        dist, needed = {start: 0}, {start: set()}
        while queue:
            pos = queue.popleft()
            for p in around(pos):
                if p in self.grid and self.grid[p] != "#" and p not in dist:
                    val = self.grid[p]
                    dist[p] = dist[pos] + 1
                    needed[p] = needed[pos] | (
                        {val.lower()} if val.isupper() else set()
                    )
                    queue.append(p)
        return {
            k: (dist[self.key_pos[k]], needed[self.key_pos[k]])
            for k in self.keys
            if self.key_pos[k] in dist
        }

    def solve(self):
        def inner(pos, got, td):
            nonlocal shortest, dp
            dp_key = (pos, got)
            if td >= shortest or td >= dp.get(dp_key, inf):
                return
            if len(got) == len(self.keys):
                if td < shortest:
                    shortest = td
                return
            dp[dp_key] = td
            for key in self.keys - got:
                if self.dist[pos][key][1] <= got:
                    if key in self.door_pos:
                        self.grid[self.door_pos[key]] = "."
                    inner(self.key_pos[key], got | {key}, td + self.dist[pos][key][0])
                    if key in self.door_pos:
                        self.grid[self.door_pos[key]] = key.upper()

        dp = {}
        shortest = inf
        inner(self.bots[0], frozenset(), 0)
        return shortest


text = data(18).read()
print(Solver(text).solve())

from aoc import *


class Solver:
    def __init__(self, text):
        self.grid = make_grid(text.splitlines())
        self.bots = [i for i, v in self.grid.items() if v == "@"]
        self.keys = {v for k, v in self.grid.items() if v.islower()}
        self.key_pos = {v: k for k, v in self.grid.items() if v.islower()}
        self.door_pos = {v.lower(): k for k, v in self.grid.items() if v.isupper()}
        self.dist = {}
        for b in self.bots:
            self.dist[b] = self.distance(b)
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
        def inner(places, got, td):
            nonlocal shortest, dp
            dp_key = (tuple(places), got)
            if td >= shortest or td >= dp.get(dp_key, inf):
                return
            if len(got) == len(self.keys):
                if td < shortest:
                    shortest = td
                return
            dp[dp_key] = td
            for bot_ix, bot in enumerate(places):
                # Things this robot can still explore.
                responsible = set(self.dist[bot].keys()) - got
                for key in responsible:
                    if self.dist[bot][key][1] <= got:
                        if key in self.door_pos:
                            self.grid[self.door_pos[key]] = "."
                        new_places = places.copy()
                        new_places[bot_ix] = self.key_pos[key]
                        inner(new_places, got | {key}, td + self.dist[bot][key][0])
                        if key in self.door_pos:
                            self.grid[self.door_pos[key]] = key.upper()

        dp = {}
        shortest = inf
        inner(self.bots, frozenset(), 0)
        return shortest


text = data(18).read()
print(Solver(text).solve())

lines = list(map(list, text.splitlines()))
mid = len(lines) // 2
lines[mid - 1][mid - 1 : mid + 2] = "@#@"
lines[mid + 1][mid - 1 : mid + 2] = "@#@"
lines[mid][mid - 1 : mid + 2] = "###"
updated = "\n".join("".join(x) for x in lines)
print(Solver(updated).solve())

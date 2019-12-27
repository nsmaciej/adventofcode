from aoc import *


class Solver:
    def __init__(self, text):
        self.grid = make_grid(text.splitlines())
        self.bots = [i for i, v in self.grid.items() if v == "@"]
        self.keys = {v for k, v in self.grid.items() if v.islower()}
        self.key_pos = {v: k for k, v in self.grid.items() if v.islower()}
        self.dp = {}
        self.dist = {}
        for b in self.bots:
            self.dist[b] = self.distance(b)
        self.responsible = {
            i: frozenset(self.dist[b].keys()) for i, b in enumerate(self.bots)
        }
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

    def _walk(self, bots, need):
        run = (tuple(bots), need)
        if not need:
            return 0
        if run in self.dp:
            return self.dp[run]
        shortest = inf
        for bot_ix, bot in enumerate(bots):
            for key in self.responsible[bot_ix] & need:
                dist, required = self.dist[bot][key]
                if required.isdisjoint(need):
                    new_bots = bots.copy()
                    new_bots[bot_ix] = self.key_pos[key]
                    total_dist = dist + self._walk(new_bots, need - {key})
                    shortest = min(total_dist, shortest)
        self.dp[run] = shortest
        return shortest

    def solve(self):
        return self._walk(self.bots, frozenset(self.keys))


text = data(18).read()
print(Solver(text).solve())

lines = list(map(list, text.splitlines()))
mid = len(lines) // 2
lines[mid - 1][mid - 1 : mid + 2] = "@#@"
lines[mid + 1][mid - 1 : mid + 2] = "@#@"
lines[mid][mid - 1 : mid + 2] = "###"
updated = "\n".join("".join(x) for x in lines)
print(Solver(updated).solve())

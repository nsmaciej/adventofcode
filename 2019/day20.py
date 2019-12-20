from aoc import *
from collections import defaultdict, deque
from pprint import pprint


def distance():
    def consider(pos, new_pos):
        if new_pos not in dist and maze[new_pos] == ".":
            # print("Dist to", new_pos, "is", dist[pos] + 1)
            dist[new_pos] = dist[pos] + 1
            path[new_pos] = path[pos] + [new_pos]
            queue.append(new_pos)

    start = portal_pos["AA"][0]
    queue = deque([start])
    dist = {start: 0}
    path = {start: [start]}
    while queue:
        pos = queue.popleft()
        if pos in near_portals:
            if near_portals[pos] == "ZZ":
                return path[pos]
            try:
                np = next(p for p in portal_pos[near_portals[pos]] if pos != p)
                consider(pos, np)
            except StopIteration:
                pass
        for new_pos in around(pos):
            consider(pos, new_pos)
    return path[portal_pos["ZZ"][0]]


maze = make_grid(data(20).read().splitlines())
near_portals = {}
portal_pos = defaultdict(list)
for pos in maze:
    if not maze[pos].isalpha():
        continue
    for m in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        p = add(pos, m)
        if p not in maze or not maze[p].isalpha():
            continue
        np = add(m, p)
        if np not in maze or maze[np] != ".":
            continue
        key = "".join(maze[x] for x in sorted([pos, p]))  # Fuck this.
        assert np not in near_portals
        near_portals[np] = key
        portal_pos[key].append(np)
# assert len(portals["AA"]) == 1 and len(portals["ZZ"]) == 1
# assert maze[portals["AA"][0]] == "." and maze[portals["ZZ"][0]] == "."

path = distance()
for p in path:
    maze[p] = "\033[1;35m*\033[0m"
maze[portal_pos["AA"][0]] = "\033[32mS\033[0m"
maze[portal_pos["ZZ"][0]] = "\033[32mE\033[0m"
print_grid(maze)
print(len(path) - 1)

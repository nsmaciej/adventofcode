from aoc import *
from collections import defaultdict, deque
from pprint import pprint


def distance(start):
    def consider(pos, new_pos):
        if new_pos not in dist and maze[new_pos] == ".":
            dist[new_pos] = dist[pos] + 1
            queue.append(new_pos)

    queue = deque([start])
    dist = {start: 0}
    while queue:
        pos = queue.popleft()
        if pos in nearby_portal and len(portal[nearby_portal[pos]]) > 1:
            dest = next(p for p in portal[nearby_portal[pos]] if pos != p)
            consider(pos, dest)
        for step in around(pos):
            consider(pos, step)
    return dist


maze = make_grid(data(20).read().splitlines())
nearby_portal = {}
portal = defaultdict(list)
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
        key = "".join(maze[x] for x in sorted([pos, p]))  # Labels follow read order.
        assert np not in nearby_portal
        nearby_portal[np] = key
        portal[key].append(np)

aa, zz = portal["AA"][0], portal["ZZ"][0]
print(distance(aa)[zz])

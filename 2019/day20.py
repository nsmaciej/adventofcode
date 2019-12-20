from aoc import *
from collections import defaultdict, deque, namedtuple
from pprint import pprint


def distance(assume_flat):
    def consider(pos, new_pos):
        if new_pos not in dist and maze[new_pos[1]] == "." and new_pos[0] >= 0:
            dist[new_pos] = dist[pos] + 1
            queue.append(new_pos)

    start = (0, portal["AA"][0].pos)
    queue = deque([start])
    dist = {start: 0}
    while queue:
        pos = queue.popleft()
        lvl, p = pos
        if nearby_portal.get(p) == "ZZ" and (lvl == 0 or assume_flat):
            return dist[pos]
        if p in nearby_portal and len(portal[nearby_portal[p]]) > 1:
            dest = next(x for x in portal[nearby_portal[p]] if p != x.pos)
            consider(pos, (0 if assume_flat else lvl + dest.level, dest.pos))
        for step in around(p):
            consider(pos, (lvl, step))
    return level, dist


Entrance = namedtuple("Entrance", ["level", "pos"])
text = data(20).read().splitlines()
width, height = len(text[0]), len(text)
maze = make_grid(text)
nearby_portal = {}
portal = defaultdict(list)
for pos in maze:
    if not maze[pos].isalpha():
        continue
    for m in around():
        p = add(pos, m)
        if p not in maze or not maze[p].isalpha():
            continue
        np = add(m, p)
        if np not in maze or maze[np] != ".":
            continue
        key = "".join(maze[x] for x in sorted([pos, p]))  # Labels follow read order.
        assert np not in nearby_portal
        nearby_portal[np] = key
        outer = np[0] < 3 or np[1] < 3 or np[0] + 3 >= height or np[1] + 3 >= width
        portal[key].append(Entrance(1 if outer else -1, np))

print(distance(True))
print(distance(False))

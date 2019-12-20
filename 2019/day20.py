from aoc import *
from collections import defaultdict, deque, namedtuple
from pprint import pprint


def distance(no_recurse):
    def consider(pos, new_pos):
        # Do not go above level zero.
        if new_pos not in dist and maze[new_pos.coord] == "." and new_pos.level >= 0:
            dist[new_pos] = dist[pos] + 1
            queue.append(new_pos)

    start = Position(0, portal["AA"][0].coord)
    queue = deque([start])
    dist = {start: 0}
    while queue:
        pos = queue.popleft()
        for step in around(pos.coord):
            consider(pos, Position(pos.level, step))
        if pos.coord not in nearby_portal:
            continue
        label = nearby_portal[pos.coord]
        if label == "ZZ" and (pos.level == 0 or no_recurse):
            return dist[pos]
        if len(portal[label]) > 1:  # This portal has another entrance.
            dest = next(x for x in portal[label] if pos.coord != x.coord)
            level_change = -1 if dest.inner else 1
            next_level = 0 if no_recurse else pos.level + level_change
            consider(pos, Position(next_level, dest.coord))
    return level, dist


Entrance = namedtuple("Entrance", ["inner", "coord"])
Position = namedtuple("Position", ["level", "coord"])
text = data(20).read().splitlines()
width, height = len(text[0]), len(text)
maze = make_grid(text)
nearby_portal = {}
portal = defaultdict(list)
for p0 in maze:
    if not maze[p0].isalpha():
        continue
    for move in around():
        p1 = add(p0, move)
        if p1 not in maze or not maze[p1].isalpha():
            continue
        # p0 and p1 are both letters. Next point in this direction is the entrance.
        entrance = add(p1, move)
        if entrance not in maze or maze[entrance] != ".":
            continue
        label = "".join(maze[x] for x in sorted([p0, p1]))  # Labels follow read order.
        assert entrance not in nearby_portal
        nearby_portal[entrance] = label
        inner = 2 < entrance[0] < height - 3 and 2 < entrance[1] < width - 3
        portal[label].append(Entrance(inner, entrance))

print(distance(True))
print(distance(False))

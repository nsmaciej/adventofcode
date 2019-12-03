def points(turns):
    times = {}
    x, y, t = 0, 0, 0
    for turn in turns.split(","):
        dx, dy = DIRECTIONS[turn[0]]
        for _ in range(int(turn[1:])):
            x += dx
            y += dy
            t += 1
            times[(x, y)] = t
    return times


DIRECTIONS = {"U": (0, 1), "D": (0, -1), "L": (-1, 0), "R": (1, 0)}
a, b = map(points, open("inputs/day03.txt"))
intersections = set(a.keys()) & set(b.keys())
print(min(map(lambda x: abs(x[0]) + abs(x[1]), intersections)))
print(min(map(lambda x: a[x] + b[x], intersections)))

def points(turns):
    times = {}
    point_set = set()
    x, y, t = 0, 0, 0
    for turn in turns.split(","):
        dx, dy = DIRECTIONS[turn[0]]
        for _ in range(int(turn[1:])):
            x += dx
            y += dy
            t += 1
            point_set.add((x, y))
            times[(x, y)] = t
    return point_set, times


a, b = open("inputs/day03.txt")
DIRECTIONS = {"U": (0, 1), "D": (0, -1), "L": (-1, 0), "R": (1, 0)}
a, at = points(a)
b, bt = points(b)
print(min(map(lambda x: abs(x[0]) + abs(x[1]), a & b)))
print(min(map(lambda x: at[x] + bt[x], a & b)))

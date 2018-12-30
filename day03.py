import re
import itertools


def parse(line):
    return tuple(map(int, re.search(r"(\d+),(\d+): (\d+)x(\d+)", line).groups()))


def drange(a, b):
    return itertools.product(range(a), range(b))


areas = list(map(parse, open("inputs/day03.txt")))
width = max(x[0] + x[2] for x in areas) + 1
height = max(x[1] + x[3] for x in areas) + 1
field = [[0] * width for _ in range(height)]
claims = set(range(len(areas)))

for area in areas:
    for dx, dy in drange(area[2], area[3]):
        field[area[0] + dx][area[1] + dy] += 1
print(sum(sum(x > 1 for x in y) for y in field))

for n, area in enumerate(areas):
    overlaps = (
        field[area[0] + dx][area[1] + dy] for dx, dy in drange(area[2], area[3])
    )
    if all(x == 1 for x in overlaps):
        print(n + 1)


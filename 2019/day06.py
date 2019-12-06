from collections import defaultdict


def count(x):
    return 1 + (count(orbits[x]) if x in orbits else 0)


def trace(x):
    return (trace(orbits[x]) if x in orbits else []) + [x]


data = [x.strip().split(")") for x in open("inputs/day06.txt")]
orbits = {}
for x, y in data:
    orbits[y] = x

print(sum(count(x) - 1 for x in orbits.keys()))
you, san = trace("YOU"), trace("SAN")
offset = sum(x == y for x, y in zip(you, san))
print(len(you) - you.index(you[offset]) + len(san) - san.index(san[offset]) - 2)

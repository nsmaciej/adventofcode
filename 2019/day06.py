def trace(x):
    return (trace(orbits[x]) if x in orbits else []) + [x]


data = [x.strip().split(")") for x in open("inputs/day06.txt")]
orbits = {y: x for x, y in data}
you, san = trace("YOU"), trace("SAN")
offset = sum(x == y for x, y in zip(you, san))
print(sum(len(trace(x)) - 1 for x in orbits.keys()))
print(len(you) - you.index(you[offset + 1]) + len(san) - san.index(san[offset + 1]))

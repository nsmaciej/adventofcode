import itertools

changes = list(map(int, open("inputs/day01.txt")))
print(sum(changes))

seen = {0}
f = 0
for x in itertools.cycle(changes):
    f += x
    if f in seen:
        break
    seen.add(f)
print(f)

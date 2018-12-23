import string


def remove(polymer, x):
    return "".join(i for i in polymer if i != x and i != x.swapcase())


def simulate(polymer):
    final = []
    i = 0
    while i < len(polymer):
        if final and final[-1] == polymer[i].swapcase():
            final.pop()
        else:
            final.append(polymer[i])
        i += 1
    return len(final)


polymer = open("inputs/day5.txt").read()
print(simulate(polymer))
to_remove = min(string.ascii_lowercase, key=lambda x: simulate(remove(polymer, x)))
print(simulate(remove(polymer, to_remove)))

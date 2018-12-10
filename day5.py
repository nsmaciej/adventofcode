import string


def invert(x):
    return x.upper() if x.islower() else x.lower()


def remove(polymer, x):
    return "".join(i for i in polymer if i != x and i != invert(x))


def simulate(polymer):
    final = []
    i = 0
    while i < len(polymer):
        if final and final[-1] == invert(polymer[i]):
            final.pop()
        else:
            final.append(polymer[i])
        i += 1
    return len(final)


polymer = open("day5.txt").read()
print(simulate(polymer))
to_remove = min(string.ascii_lowercase, key=lambda x: simulate(remove(polymer, x)))
print(simulate(remove(polymer, to_remove)))

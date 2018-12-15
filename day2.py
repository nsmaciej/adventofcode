from collections import Counter


def value(x):
    counts = set(Counter(x).values())
    return 2 in counts, 3 in counts


def common_letters():
    for x in data:
        for y in data:
            if sum(i != j for i, j in zip(x, y)) == 1:
                return "".join(i for i, j in zip(x, y) if i == j)


data = [x.strip() for x in open("inputs/day2.txt")]
x, y = map(sum, zip(*map(value, data)))
print(x * y)
print(common_letters())

from itertools import product
from collections import Counter


def cmp(x, y):
    for i, j in zip(x, y):
        if i == j:
            continue
        return (i > j) - (i < j)
    return 0


def solve(group):
    answers = 0
    for attempt in product(range(10), repeat=len(start)):
        if (
            cmp(attempt, start) >= 0
            and cmp(attempt, end) <= 0
            and tuple(sorted(attempt)) == attempt
            and any(group(x) for x in Counter(attempt).values())
        ):
            answers += 1
    return answers


start, end = map(
    lambda x: list(map(int, x.strip())), open("inputs/day04.txt").read().split("-")
)
print(solve(lambda x: x >= 2))
print(solve(lambda x: x == 2))

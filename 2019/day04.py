from collections import Counter


def solve(group):
    for x in range(start, end + 1):
        digits = list(str(x))
        yield (
            x >= start
            and x <= end
            and sorted(digits) == digits
            and any(group(i) for i in Counter(digits).values())
        )


start, end = map(int, open("inputs/day04.txt").read().split("-"))
print(sum(solve(lambda x: x >= 2)))
print(sum(solve(lambda x: x == 2)))

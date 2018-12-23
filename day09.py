import re
from collections import deque


def solve(players, last_marble):
    circle = deque([0])
    score = [0] * players
    for marble in range(1, last_marble + 1):
        if marble % 23 == 0:
            player = (marble - 1) % players
            circle.rotate(7)
            score[player] += circle.popleft() + marble
        else:
            circle.rotate(-2 if len(circle) > 2 else 0)
            circle.appendleft(marble)
    return max(score)


data = open("inputs/day9.txt").read()
players, last_marble = map(
    int, re.match(r"(\d+) players; last marble is worth (\d+) points", data).groups()
)
print(solve(players, last_marble))
print(solve(players, last_marble * 100))

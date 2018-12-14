import re
import array

players, points = map(
    int,
    re.match(
        r"(\d+) players; last marble is worth (\d+) points", open("day9.txt").read()
    ).groups(),
)

score = [0] * players
circle = array.array("i", [0])
i = 0
for marble in range(1, points + 1):
    if marble % 23 == 0:
        player = (marble - 1) % players
        new_i = (i - 7) % len(circle)
        score[player] += circle.pop(new_i) + marble
        i = new_i
    else:
        i = (i + 2) % len(circle) if len(circle) > 2 else 1
        circle.insert(i, marble)

print(max(score))

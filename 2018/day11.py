import numpy as np
import itertools


def power_level(pos):
    x, y = pos
    rack_id = 10 + x
    return ((rack_id * y + serial) * rack_id // 100 % 10) - 5


def square_sum(x, y, k):
    return area[x + k, y + k] + area[x, y] - area[x, y + k] - area[x + k, y]


def max_sum(k):
    return max(np.ndindex(size - k, size - k), key=lambda h: square_sum(*h, k))


def show(*vals):
    print(",".join(map(str, vals)))


serial = int(open("inputs/day11.txt").read())
size = 300
levels = np.apply_along_axis(power_level, 0, np.indices((size, size)))
area = np.pad(levels.cumsum(0).cumsum(1), 1, "constant")
show(*max_sum(3))
best_k = max(range(size), key=lambda k: square_sum(*max_sum(k), k))
show(*max_sum(best_k), best_k)

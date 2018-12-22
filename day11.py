import numpy as np
import itertools


def power_level(pos):
    x, y = pos
    rack_id = 10 + x
    return ((rack_id * y + serial) * rack_id // 100 % 10) - 5


def sum_between(s, e):
    # Usually each coordinate would be one off, but padding counteracts that.
    return area[e[0], e[1]] + area[s[0], s[1]] - area[s[0], e[1]] - area[e[0], s[1]]


def max_sum(k):
    return max(
        np.ndindex(size - k, size - k),
        key=lambda h: sum_between(h, (h[0] + k, h[1] + k)),
    )


def show(*vals):
    print(",".join(map(str, vals)))


serial = int(open("inputs/day11.txt").read())
size = 300
levels = np.apply_along_axis(power_level, 0, np.indices((size, size)))
area = np.pad(levels.cumsum(0).cumsum(1), 1, "constant")
best_k = max(range(size), key=max_sum)
show(*max_sum(3))
show(*max_sum(best_k), best_k)

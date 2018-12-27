import numpy as np
from collections import Counter


def closest(x):
    distances = abs(coords - x).sum(1)
    best = distances.argmin()
    if sum(distances == distances[best]) > 1:
        return -1
    return best


def within_10000(x):
    return sum(abs(coords - x).sum(1)) < 10000


def edge_set(a):
    return set(a[:, [0, -1]].flat) | set(a[[0, -1]].flat) | {-1}


coords = np.array([tuple(map(int, x.split(", "))) for x in open("inputs/day6.txt")])
width = max(coords[:, 0]) + 1
height = max(coords[:, 1]) + 1
voronoi = np.apply_along_axis(closest, 0, np.indices((width, height)))
infinite = edge_set(voronoi)
print(Counter(x for x in voronoi.flat if x not in infinite).most_common(1)[0][1])
print(np.apply_along_axis(within_10000, 0, np.indices((width, height))).sum())

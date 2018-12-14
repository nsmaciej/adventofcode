import re
import itertools
from heapq import *


def parse(x):
    return re.match(
        r"Step (\w) must be finished before step (\w) can begin\.", x
    ).groups()


def group(data):
    r = {}
    for x in data:
        r.setdefault(x[1], set()).add(x[0])
    return r


def invert(data):
    r = {}
    for k, v in data.items():
        for x in v:
            r.setdefault(x, set()).add(k)
    return r


def roots(deps):
    return set(itertools.chain.from_iterable(deps.values())) - set(deps.keys())


def indegrees(deps):
    return {k: len(v) for k, v in deps.items()}


def solve(deps):
    indeg = indegrees(deps)
    inv = invert(deps)
    queue = list(roots(deps))
    heapify(queue)
    while queue:
        x = heappop(queue)
        yield x
        if x not in inv:
            break
        for i in inv[x]:
            indeg[i] -= 1
            if indeg[i] == 0:
                heappush(queue, i)


def duration(job):
    return 61 + ord(job) - ord("A")


def simulate(deps):
    workers = 5
    working = [(0, None)] * workers
    queue = list(roots(deps))
    indeg = indegrees(deps)
    inv = invert(deps)
    tt = 0
    while queue or any(j for _, j in working):
        for i in range(workers):
            if not queue:
                break
            if working[i][0] <= 0:
                node = queue.pop()
                working[i] = (duration(node), node)

        dt = min(x[0] for x in working if x[1] is not None)
        tt += dt

        print("".join(a[1] or "." for a in working))
        for i, (t, j) in enumerate(working):
            if j is None:
                continue
            working[i] = (t - dt, j)
            if t - dt <= 0:
                working[i] = (0, None)
                if j not in inv:
                    continue
                for k in inv[j]:
                    indeg[k] -= 1
                    if indeg[k] == 0:
                        queue.append(k)
    return tt


deps = group(map(parse, open("day7.txt")))
print("".join(solve(deps)))
print(simulate(deps))

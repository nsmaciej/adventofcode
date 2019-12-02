import re
from functools import reduce
from itertools import islice


def parse(x):
    return re.match(r"([\.#]{5}) => ([\.#])", x).groups()


def scan(state):
    l = f"...{state}..."
    for i in range(3, 3 + len(state)):
        yield l[i - 2 : i + 3]


def simulate(state):
    offset = 0
    while True:
        if state[0] == "#":
            offset += 1
            state = "." + state
        if state[-1] == "#":
            state += "."
        state = "".join(transition.get(x, ".") for x in scan(state))
        yield offset, state


def pot_sum(offset, state):
    return sum(i - offset for i, x in enumerate(state) if x == "#")


f = open("inputs/day12.txt")
pots = re.match(r"initial state: ([\.#]+)", next(f))[1]
next(f)
transition = dict(map(parse, f))

final_offset, final_state = next(islice(simulate(pots), 19, None))
print(pot_sum(final_offset, final_state))

# Spot when the pots converge to just shifting right.
prev_state = (0, "")
for step, state in enumerate(simulate(pots)):
    if prev_state[0] == state[0] and prev_state[1] == state[1][1:]:
        break
    prev_state = state
print(pot_sum(prev_state[0] - 50_000_000_000 + step, prev_state[1]))

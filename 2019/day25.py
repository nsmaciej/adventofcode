from aoc import *
from intcode import Program, Vm
from itertools import combinations
import re

# Worked out with a map.
all_items = """
north
west
take mug
west
take easter egg
east
east
south
south
take asterisk
south
west
north
take jam
south
east
north
east
take klein bottle
south
west
take tambourine
west
take cake
east
south
east
take polygon
"""


def run(line):
    droid.input_line(line)
    droid.run()
    return droid.drain_output()


def response(line):
    return "".join(map(chr, run(line)))


def solve():
    for line in all_items.splitlines():
        droid.input_line(line)
    droid.run()
    droid.drain_output()
    assert "Security Checkpoint" in response("north")

    items = [x[2:] for x in response("inv").splitlines() if x.startswith("-")]
    last_c = 0
    for ix in range(1, 2 ** len(items)):
        c = ix ^ (ix >> 1)  # Gray code.
        item_ix = len(bin(last_c ^ c)) - 3  # Find the index of the flipped bit.
        action = "drop" if c & (last_c ^ c) else "take"
        run(f"{action} {items[item_ix]}")
        last_c = c
        if "you are ejected back to the checkpoint" not in (r := response("east")):
            return re.search(f"[0-9]+", r)[0]


droid = Vm(data(25).read())
print(solve())

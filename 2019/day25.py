from aoc import *
from intcode import Program, Vm, show_output
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
north
"""

def response(line):
    droid.input_line(line)
    droid.run()
    return show_output(droid.drain_output())


def solve():
    for line in all_items.splitlines():
        droid.input_line(line)
    droid.run()
    droid.drain_output()

    items = set(x[2:] for x in response("inv").splitlines() if x.startswith("-"))
    for item_count in range(1, len(items)):
        for inv in combinations(items, item_count):
            to_drop = items - set(inv)
            for item in to_drop:
                response(f"drop {item}")
            if "you are ejected back to the checkpoint" not in (r := response("east")):
                return re.search(f"[0-9]+", r)[0]
            else:
                for item in to_drop:
                    response(f"take {item}")

droid = Vm(data(25).read())
print(solve())
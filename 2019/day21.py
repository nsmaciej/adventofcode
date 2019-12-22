from aoc import *
from intcode import Vm


def run(program):
    droid = Vm(data(21).read())
    for line in program.strip().splitlines():
        droid.input_line(line)
    print(droid.complete()[-1])


# D ^ ~C ^ ~A
run(
    """
NOT C J
AND D J
NOT A T
OR T J
WALK
"""
)


# D ^ H ^ ~(B ^ C) ^ ~A
run(
    """
OR B T
AND C T
NOT T J
AND D J
AND H J
NOT A T
OR T J
RUN
"""
)

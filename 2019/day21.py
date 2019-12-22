from aoc import *
from intcode import Vm, show_output

# OR is like 'if x: y = true'
# AND is like 'if y: y = x'
# NOT is like 'x = not y'


def run(program):
    droid = Vm(data(21).read())
    for line in program.strip().splitlines():
        if not line.startswith("#") and line:
            droid.input_line(line)
    print(droid.complete()[-1])


# D ^ ~C ^ ~A
run(
    """
NOT C J
AND D J
# Jump if: A air
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

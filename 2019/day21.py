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
    return droid.complete()


print(
    run(
        """
# Jump if: C air, D land
NOT C J
AND D J
# Jump if: A air
NOT A T
OR T J
WALK
"""
    )[-1]
)

print(
    run(
        """
# @#..#.###        @#..L.##L
# @##.##.##        @##.L#_#L
# @##.##..#        @##.L#..L
# @##.#..##        @##.L..#L
# @#.##.###        @#.#L..#L (note: C and F not air)
# @##.###.#        @##.###.#
# @##.#.##.        @##.L.##.       (not)
# @####.###(.##.)  @###L.##L(.##.) (not)
# @####..##(.##.)  @###L..#L(.##.) (not)

# Jump if we have somewhere to land, again so one jump later and there is some air to
# the left and right of the first land block. Otherwise jump if on edge.
# D=land and H=land and (B=air or C=air) and (E=air or F=air or G=air)
# D ^ H ^ (~B v ~C) ^ (~E v ~F v ~G)
# D ^ H ^ ~(B ^ C) ^ ~(E ^ F ^ G)

OR B T
AND C T
NOT T J

OR E T
AND F T
AND G T
NOT T T
AND T J

AND D J
AND H J

NOT A T
OR T J
RUN
"""
    )[-1]
)


from aoc import *
from intcode import Vm


def show_output(output):
    def try_chr(x):
        if x < 128:
            c = chr(x)
            if c.isprintable() or c == "\n":
                return c
        return f"<{x}>"

    return "".join(map(try_chr, output))


droid = Vm(data(21).read())
# The droid can jump over holes three wide.
program = """
NOT C J
AND D J
NOT A T
OR T J
WALK
"""
for line in program.strip().splitlines():
    droid.input_line(line)
droid.run()
print(show_output(droid.drain_output()))

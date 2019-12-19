from intcode import Vm
import argparse
import sys


def parse_overrides(overrides):
    return tuple(map(int, overrides.split(":")))


def show_output(output):
    output = map(chr, output)
    return "".join(
        x if x.isascii() and (x.isprintable() or x == "\n") else f"<{ord(x)}>"
        for x in output
    )


parser = argparse.ArgumentParser()
parser.add_argument("-r", "--raw", help="do not use ascii I/O", action="store_true")
parser.add_argument("-i", "--inputs", help="provide input in advance", nargs="+")
parser.add_argument(
    "-t", "--tape", help="tape overrides", nargs="+", default=[], type=parse_overrides
)
parser.add_argument("program", help="program to run")
args = parser.parse_args()

vm = Vm(open(args.program).read())
for pos, val in args.tape:
    vm.set_tape(pos, val)

while True:
    halted = vm.run()
    output = vm.drain_output()
    if args.raw:
        # In raw mode we might get no output, no point printing nothing.
        if output:
            print(" ".join(map(str, output)))
    else:
        print(show_output(output), end="")
    if halted:
        break

    data = input() if args.inputs is None else args.inputs.pop(0)
    if args.raw:
        vm.input(int(data))
    else:
        vm.input_line(data)
print()  # Make sure we end on a newline.

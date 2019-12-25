from intcode import Vm
import argparse
import sys


def parse_overrides(overrides):
    return tuple(map(int, overrides.split(":")))


def try_chr(x):
    if x < 128:
        c = chr(x)
        if c.isprintable() or c == "\n":
            return c
    return f"<{x}>"


parser = argparse.ArgumentParser()
parser.add_argument("-r", "--raw", help="do not use ascii I/O", action="store_true")
parser.add_argument("-i", "--inputs", help="provide input in advance", nargs="+")
parser.add_argument("-l", "--log", help="log the session")
parser.add_argument(
    "-t", "--tape", help="tape overrides", nargs="+", default=[], type=parse_overrides
)
parser.add_argument("program", help="program to run")
args = parser.parse_args()

vm = Vm(open(args.program).read())
for pos, val in args.tape:
    vm.set_tape(pos, val)
log = open(args.log, "w") if args.log else None


while True:
    halted = vm.run()
    output = vm.drain_output()
    if args.raw:
        # In raw mode we might get no output, no point printing nothing.
        if output:
            print(" ".join(map(str, output)))
    else:
        print("".join(map(try_chr, output)), end="")
    if halted:
        break

    data = input() if args.inputs is None else args.inputs.pop(0)
    if log:
        log.write(data + "\n")
    if args.raw:
        vm.input(int(data))
    else:
        vm.input_line(data)
print()  # Make sure we end on a newline.

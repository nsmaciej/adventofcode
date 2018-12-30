import re
from operator import *


def meta_op(fn):
    def impl(args, state):
        try:
            value = int(fn(s=state, a=args[1], b=args[2]))
            output = args[3]
            return state[:output] + (value,) + state[output + 1 :]
        except IndexError:
            return None

    return impl


def reg_op(op):
    return meta_op(lambda s, a, b: op(s[a], s[b]))


def imm_op(op, *, reverse=False):
    return meta_op(lambda s, a, b: op(a, s[b]) if reverse else op(s[a], b))


def int_tuple(line, sep):
    return tuple(map(int, line.split(sep)))


def parse(match):
    before, op, after = match.groups()
    return int_tuple(before, ", "), int_tuple(op, " "), int_tuple(after, ", ")


def find_option(iterable, fn):
    for x in iterable:
        if fn(x):
            return x
    for opcode, options in possible:
        opcode


ops = {
    "addr": reg_op(add),
    "addi": imm_op(add),
    "mulr": reg_op(mul),
    "muli": imm_op(mul),
    "banr": reg_op(and_),
    "bani": imm_op(and_),
    "borr": reg_op(or_),
    "bori": imm_op(or_),
    "setr": meta_op(lambda s, a, b: s[a]),
    "seti": meta_op(lambda s, a, b: a),
    "gtir": imm_op(gt, reverse=True),
    "grri": imm_op(gt),
    "grrr": reg_op(gt),
    "eqir": imm_op(eq, reverse=True),
    "eqri": imm_op(eq),
    "eqrr": reg_op(eq),
}

contents = open("inputs/day16.txt").read()
effects = re.finditer(
    r"Before:\s+\[([\d\s,]+)\]\n([\d ]+)\nAfter:\s+\[([\d\s,]+)\]",
    contents,
    re.MULTILINE,
)

answer = 0
stats = {}
possible = [set() for _ in range(16)]
for before, args, after in map(parse, effects):
    viable = 0
    for name, op in ops.items():
        if op(args, before) == after:
            viable += 1
            possible[args[0]].add(name)
    viable = sum(op(args, before) == after for op in ops.values())
    if viable >= 3:
        answer += 1
print(answer)

# Elimate possiblities
opcodes = {}
while any(possible):
    opcode, name = next((k, v.pop()) for k, v in enumerate(possible) if len(v) == 1)
    for options in possible:
        options.discard(name)
    opcodes[opcode] = name

offset = contents.find("\n\n\n")
data = (int_tuple(x, " ") for x in contents[offset:].split("\n") if x)
state = (0,) * 4
for args in data:
    state = ops[opcodes[args[0]]](args, state)
print(state[0])

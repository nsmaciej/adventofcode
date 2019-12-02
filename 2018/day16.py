import re
import elfcode


def int_list(line, sep):
    return list(map(int, line.split(sep)))


def parse(match):
    before, op, after = match.groups()
    return int_list(before, ", "), int_list(op, " "), int_list(after, ", ")


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
    for name, op in elfcode.ops.items():
        if op(args, before) == after:
            viable += 1
            possible[args[0]].add(name)
    viable = sum(op(args, before) == after for op in elfcode.ops.values())
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
data = (int_list(x, " ") for x in contents[offset:].split("\n") if x)
state = [0] * 4
for args in data:
    state = elfcode.ops[opcodes[args[0]]](args, state)
print(state[0])

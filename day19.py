import re
import elfcode


def parse(line):
    args = line.split()
    return [args[0]] + list(map(int, args[1:]))


def optimise(instructions):
    return instructions

def show(args):
    # mulr 5 3 4 {4}
    # eqrr 4 1 4 {5}
    # addr 4 2 2 {6}
    # addi 2 1 2 {8}
    # addr 5 0 0 {}
    # addi 3 1 3 {9}
    # gtrr 3 1 4 {10}
    # addr 2 4 2 {11}
    # seti 2 1 2 {3}
    # Let's call this idom recognition




def simulate(reg0):
    state = [0] * 6
    state[0] = reg0
    profile = [0 for _ in range(len(instructions))]
    jumps = [set() for _ in range(len(instructions))]
    counter = 0
    while state[pc_reg] < len(instructions) and counter < 10_000_000:
        old_pc = state[pc_reg]
        if old_pc == 2:
            print(state)
        args = instructions[state[pc_reg]]
        profile[state[pc_reg]] += 1
        counter += 1
        state = elfcode.ops[args[0]](args, state)
        state[pc_reg] += 1
        jumps[old_pc].add(state[pc_reg])
    for i, count in enumerate(profile):
        print(f"{i:>2}: {count:>10}   {contents[i + 1].strip()}\t{jumps[i]}")
    return state[0]


contents = open("inputs/day19.txt").readlines()
pc_reg = int(re.match(r"#ip (\d+)", contents[0]).group(1))
instructions = optimise(list(map(parse, contents[1:])))
print(simulate(0))
print(simulate(1))

from intcode import Vm

program = open("inputs/day09.txt").read()
print(Vm(program, [1]).complete()[-1])
print(Vm(program, [2]).complete()[-1])

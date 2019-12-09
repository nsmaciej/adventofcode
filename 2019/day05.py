from intcode import Vm

program = open("inputs/day05.txt").read()
print(Vm(program, [1]).complete()[-1])
print(Vm(program, [5]).complete()[-1])

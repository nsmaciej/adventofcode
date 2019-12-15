from intcode import Vm
from aoc import *

program = data(9).read()
print(Vm(program, [1]).complete()[-1])
print(Vm(program, [2]).complete()[-1])

from intcode import Vm
from aoc import *

program = data(5).read()
print(Vm(program, [1]).complete()[-1])
print(Vm(program, [5]).complete()[-1])

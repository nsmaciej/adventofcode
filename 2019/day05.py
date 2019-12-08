import operator
from intcode import Vm


def simulate(test_id):
    vm = Vm(tape, [test_id])
    assert vm.run()
    return vm.outputs[-1]


tape = list(map(int, open("inputs/day05.txt").read().split(",")))
print(simulate(1))
print(simulate(5))

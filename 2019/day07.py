from itertools import permutations, cycle
from intcode import Vm


def signal(phases):
    amps = [Vm(program, [x]) for x in phases]
    signal = 0
    for i, amp in cycle(enumerate(amps)):
        amp.inputs.append(signal)
        if amp.run() and i == 4:
            return amp.outputs.pop()
        signal = amp.outputs.pop()


program = open("inputs/day07.txt").read()
print(max(map(signal, permutations(range(5)))))
print(max(map(signal, permutations(range(5, 10)))))

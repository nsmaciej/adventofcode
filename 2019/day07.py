from itertools import permutations, cycle
from intcode import Vm


def signal(phases):
    amps = [Vm(program, [x]) for x in phases]
    signal = 0
    for i, amp in cycle(enumerate(amps)):
        amp.input(signal)
        if amp.run() and i == 4:
            return amp.output()
        signal = amp.output()


program = open("inputs/day07.txt").read()
print(max(map(signal, permutations(range(5)))))
print(max(map(signal, permutations(range(5, 10)))))

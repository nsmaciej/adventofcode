from itertools import permutations
from intcode import Vm


def signal(phases):
    amps = [Vm(tape, [x]) for x in phases]
    signal = 0
    amp_ix = 0
    while True:
        amp = amps[amp_ix]
        amp.inputs.append(signal)
        if amp.run() and amp_ix == 4:
            return amp.outputs.pop()
        signal = amp.outputs.pop()
        amp_ix = (amp_ix + 1) % len(amps)


tape = list(map(int, open("inputs/day07.txt").read().split(",")))
print(max(map(signal, permutations(range(5)))))
print(max(map(signal, permutations(range(5, 10)))))

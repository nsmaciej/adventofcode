from intcode import Vm


def simulate(n, v):
    vm = Vm(tape)
    vm.tape[1] = n
    vm.tape[2] = v
    assert vm.run()
    return vm.tape[0]


tape = list(map(int, open("inputs/day02.txt").read().split(",")))
print(simulate(12, 2))
for n in range(100):
    for v in range(100):
        if simulate(n, v) == 19690720:
            print(100 * n + v)
            break

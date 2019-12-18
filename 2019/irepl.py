from intcode import Vm
import sys

vm = Vm(open(sys.argv[1]).read())
vm.set_tape(0, 2)
while not vm.run():
    print("".join(map(chr, vm.drain_output())), end="")
    vm.input_line(input())
print("".join(map(chr, vm.drain_output())), end="")

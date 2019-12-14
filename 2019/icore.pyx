# distutils: language = c++
# cython: cdivision=True, boundscheck=False, wraparound=False

from libc.stdint cimport int64_t
from libcpp.vector cimport vector

cdef class Program:
    cdef vector[int64_t] program

    def __init__(self, str program):
        self.program = map(int, program.split(","))


cdef class Vm:
    cdef list outputs
    cdef list inputs
    cdef int64_t pc 
    cdef int64_t base
    cdef vector[int64_t] tape

    def __init__(self, program, inputs=None):
        self.outputs = []
        self.inputs = inputs or []
        self.pc = 0
        self.base = 0
        self.tape = (
            (<Program>program).program
            if isinstance(program, Program)
            else map(int, program.split(","))
        )

    cdef inline void allocate_up_to(self, int64_t ix):
        while <size_t>ix >= self.tape.size():
            self.tape.resize(self.tape.size() * 2)

    cdef inline void store(self, int64_t ix, int64_t value):
        self.allocate_up_to(ix)
        self.tape[ix] = value

    cdef inline int64_t get(self, int64_t ix):
        self.allocate_up_to(ix)
        return self.tape[ix]

    cdef inline int64_t arg_index(self, int arg):
        cdef int ix = self.pc + 1 + arg
        cdef int[3] positions = [100, 1000, 10000]
        cdef int mode = self.get(self.pc) // positions[arg] % 10
        if mode == 0:  # Indirect.
            return self.get(ix)
        elif mode == 1:  # Immediate.
            return ix
        elif mode == 2:  # Relative.
            return self.base + self.get(ix)

    cdef inline int64_t arg(self, int arg):
        return self.get(self.arg_index(arg))

    def set_tape(self, int64_t ix, int64_t value):
        self.store(ix, value)

    def get_tape(self, int64_t ix):
        return self.get(ix)

    def has_output(self):
        return bool(self.outputs)

    def output(self):
        return self.outputs.pop(0)

    def drain_output(self):
        output = self.outputs
        self.outputs = []
        return output

    def input(self, value):
        self.inputs.append(value)

    def complete(self):
        assert self.run()
        return self.drain_output()

    cpdef bint run(self):
        cdef int inst, op
        while self.get(self.pc) != 99:
            inst = self.get(self.pc)
            op = inst % 100

            # Math operators.
            if op == 1:
                self.store(self.arg_index(2), self.arg(0) + self.arg(1))
                self.pc += 4
            elif op == 2:
                self.store(self.arg_index(2), self.arg(0) * self.arg(1))
                self.pc += 4
            elif op == 7:
                self.store(self.arg_index(2), self.arg(0) < self.arg(1))
                self.pc += 4
            elif op == 8:
                self.store(self.arg_index(2), self.arg(0) == self.arg(1))
                self.pc += 4

            # Jumps.
            elif op == 5:
                if self.arg(0):
                    self.pc = self.arg(1)
                else:
                    self.pc += 3
            elif op == 6:
                if not self.arg(0):
                    self.pc = self.arg(1)
                else:
                    self.pc += 3

            # I/O.
            elif op == 3:
                if not self.inputs:
                    # Need more input.
                    return False
                self.store(self.arg_index(0), self.inputs.pop(0))
                self.pc += 2
            elif op == 4:
                self.outputs.append(self.arg(0))
                self.pc += 2

            # Relative base
            elif op == 9:
                self.base += self.arg(0)
                self.pc += 2

            else:
                assert op == 99
        return True
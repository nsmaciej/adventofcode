from operator import *

__all__ = ["ops"]


def meta_op(fn):
    def impl(args, state):
        try:
            state = state[:]
            state[args[3]] = int(fn(s=state, a=args[1], b=args[2]))
            return state
        except IndexError:
            return None

    return impl


def reg_op(op):
    return meta_op(lambda s, a, b: op(s[a], s[b]))


def imm_op(op, *, reverse=False):
    return meta_op(lambda s, a, b: op(a, s[b]) if reverse else op(s[a], b))


ops = {
    "addr": reg_op(add),
    "addi": imm_op(add),
    "mulr": reg_op(mul),
    "muli": imm_op(mul),
    "banr": reg_op(and_),
    "bani": imm_op(and_),
    "borr": reg_op(or_),
    "bori": imm_op(or_),
    "setr": meta_op(lambda s, a, b: s[a]),
    "seti": meta_op(lambda s, a, b: a),
    "gtir": imm_op(gt, reverse=True),
    "gtri": imm_op(gt),
    "gtrr": reg_op(gt),
    "eqir": imm_op(eq, reverse=True),
    "eqri": imm_op(eq),
    "eqrr": reg_op(eq),
}

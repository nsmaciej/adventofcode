import numpy as np


def apply(array, ix, f):
    array[ix] = f(array[ix])


def show(yx):
    print(f"{yx[1]},{yx[0]}")


board = np.array([list(x[:-1]) for x in open("inputs/day13.txt").readlines()[:-1]])
arrows = {">": (0, 1), "<": (0, -1), "^": (-1, 0), "v": (1, 0)}
carts = np.transpose(np.isin(board, list("><^v")).nonzero())
delta = np.array([arrows[x] for x in board[tuple(carts.T)]])
turn = np.zeros(len(carts), int)
board[np.isin(board, list("><"))] = "-"
board[np.isin(board, list("^v"))] = "|"
crashes = []

while len(carts) > 1:
    mask = np.ones(len(carts), bool)
    for i in np.lexsort(np.rot90(carts)):
        carts[i] += delta[i]
        unique, counts = np.unique(carts[mask], axis=0, return_counts=True)
        if counts.max() > 1:
            crash = unique[counts > 1][0]
            mask &= ~(carts == crash).all(axis=1)
            crashes.append(crash)

    carts, turn, delta = carts[mask], turn[mask], delta[mask]
    over = board[tuple(carts.T)]
    apply(delta, over == "/", lambda x: x @ [[0, -1], [-1, 0]])
    apply(delta, over == "\\", lambda x: x @ [[0, 1], [1, 0]])
    apply(delta, (over == "+") & (turn == 2), lambda x: x @ [[0, -1], [1, 0]])
    apply(delta, (over == "+") & (turn == 0), lambda x: x @ [[0, 1], [-1, 0]])
    apply(turn, over == "+", lambda x: (x + 1) % 3)

show(crashes[0])
show(carts[0])

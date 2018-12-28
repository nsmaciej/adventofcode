import numpy as np
from heapq import *


def show(board):
    if PRINT:
        colours = {"E": "\x1b[32mE\x1b[0m", "G": "\x1b[31mG\x1b[0m"}
        print("\n".join("".join(colours.get(y, y) for y in x) for x in board))


def spread(m):
    r = np.roll(m, 1, 0) | np.roll(m, -1, 0)
    r |= np.roll(m, 1, 1) | np.roll(m, -1, 1)
    return r & ~m


def adjacent(pt):
    # Returned in read order.
    return ((pt[0] + y, pt[1] + x) for y, x in [(-1, 0), (0, -1), (0, 1), (1, 0)])


def distance(board, pt):
    queue = [(0, pt)]
    r = np.full(board.shape, INF)
    r[pt] = 0
    while queue:
        dist, pos = heappop(queue)
        if r[pos] < dist:
            continue
        for adj in adjacent(pos):
            if board[adj] == "." and r[adj] > 1 + dist:
                r[adj] = 1 + dist
                heappush(queue, (1 + dist, adj))
    return r


def choose_step(board, pt):
    if any(board[x] == ENEMY[board[pt]] for x in adjacent(pt)):
        return None  # Already near enemy
    options = spread(board == ENEMY[board[pt]]) & (board == ".")
    if not options.max():
        return None  # Nothing is reachable.
    pt_dist = distance(board, pt)[options]
    target = tuple(np.argwhere(options)[pt_dist == pt_dist.min()][0])
    target_dist = distance(board, target)
    step = min(adjacent(pt), key=lambda x: target_dist[x])
    if target_dist[step] == INF:
        return None  # Cannot move.
    return step


def entities(board):
    return np.isin(board, list("EG"))


def solve(board, elf_attack):
    hp = np.zeros(board.shape, int)
    hp[entities(board)] = 200
    turn = 0
    alive = {"E": (board == "E").sum(), "G": (board == "G").sum()}
    deaths = {"E": 0, "G": 0}
    while True:
        order = list(map(tuple, np.argwhere(entities(board))))
        for pos in order:
            if board[pos] == ".":
                continue  # Got killed.
            enemy = ENEMY[board[pos]]
            step = choose_step(board, pos)
            if step is not None:
                board[step], board[pos] = board[pos], "."
                hp[step] = hp[pos]
                pos = step
                show(board)
            enemies = [x for x in adjacent(pos) if board[x] == enemy]
            if enemies:
                attack = min(enemies, key=lambda x: hp[x])
                hp[attack] -= elf_attack if board[pos] == "E" else 3
                if hp[attack] <= 0:
                    deaths[enemy] += 1
                    board[attack] = "."
                    show(board)
        if alive["E"] == deaths["E"] or alive["G"] == deaths["G"]:
            return hp[entities(board)].sum() * turn, deaths
        turn += 1


PRINT = False
INF = 0xFFFF
ENEMY = {"E": "G", "G": "E"}
board = np.array([list(x.strip()) for x in open("inputs/day15.txt")])

print(solve(board.copy(), 3)[0])

elf_attack = 4
while True:
    answer, deaths = solve(board.copy(), elf_attack)
    if deaths["E"] == 0:
        print(answer)
        break
    elf_attack += 1

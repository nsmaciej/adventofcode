import numpy as np
from collections import deque


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


def board_select(board, pt, fn):
    queue = deque([(0, pt)])
    visited = np.zeros(board.shape, bool)
    visited[pt] = True
    selected = (INF, pt)
    while queue:
        dist, pos = queue.popleft()
        if board[pos] == "." and fn(pos):
            selected = min(selected, (dist, pos))
        if dist >= selected[0]:
            continue  # No point checking further.
        for adj in adjacent(pos):
            if board[adj] == "." and not visited[adj]:
                visited[adj] = True
                queue.append((1 + dist, adj))
    if fn(selected[1]):
        return selected[1]
    return None


def choose_step(board, pt):
    enemy = ENEMY[board[pt]]
    if any(board[x] == enemy for x in adjacent(pt)):
        # Already near enemy, board_select would fail since we occupy the spot.
        return None
    in_range = spread(board == enemy)
    target = board_select(board, pt, lambda x: in_range[x])
    if target is None:
        return None  # No reachable target.
    valid_moves = set(adjacent(pt))
    return board_select(board, target, lambda x: x in valid_moves)


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
            # Attack
            attack = min(
                (x for x in adjacent(pos) if board[x] == enemy),
                key=lambda x: hp[x],
                default=None,
            )
            if attack is not None:
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

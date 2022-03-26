import numpy as np
from collections import deque


def adjacent_cells_mask(m):
    r = np.roll(m, 1, 0) | np.roll(m, -1, 0) | np.roll(m, 1, 1) | np.roll(m, -1, 1)
    return r & ~m


def adjacent_points(pt):
    # Returned in read order.
    return ((pt[0] + y, pt[1] + x) for y, x in [(-1, 0), (0, -1), (0, 1), (1, 0)])


def find_nearest_filter(board, pt, predicate):
    inf = 0xFFFF
    queue = deque([(0, pt)])
    visited = np.zeros_like(board, bool)
    visited[pt] = True
    selected = (inf, pt)
    while queue:
        dist, pos = queue.popleft()
        if predicate(pos):
            selected = min(selected, (dist, pos))  # Break ties using reading order.
        if dist >= selected[0]:
            continue  # We will never find a better dist.
        for adj in adjacent_points(pos):
            if board[adj] == "." and not visited[adj]:
                visited[adj] = True
                queue.append((1 + dist, adj))
    if selected[0] != inf:
        return selected[1]
    return None


def find_next_step(board, pt, enemy):
    if any(board[x] == enemy for x in adjacent_points(pt)):
        # Already near enemy, since pt is not empty, code below would skip it.
        return None
    in_range = adjacent_cells_mask(board == enemy)
    if enemy := find_nearest_filter(board, pt, lambda x: in_range[x]):
        valid_moves = set(adjacent_points(pt))
        return find_nearest_filter(board, enemy, lambda x: x in valid_moves)
    return None  # No reachable target.


def units_mask(board):
    return np.isin(board, ["E", "G"], assume_unique=True)  # Returns reading order.


def solve(board, elf_attack):
    enemy_for_unit = {"E": "G", "G": "E"}
    hp = np.zeros_like(board, int)
    hp[units_mask(board)] = 200
    turn = 0
    alive = {"E": np.count_nonzero(board == "E"), "G": np.count_nonzero(board == "G")}
    while True:
        for pos in map(tuple, np.argwhere(units_mask(board))):
            if board[pos] == ".":
                continue  # Was alive during argwhere, but got killed.
            enemy = enemy_for_unit[board[pos]]
            if step := find_next_step(board, pos, enemy):  # Consider moving.
                board[step], board[pos] = board[pos], "."
                hp[step], hp[pos] = hp[pos], 0
                pos = step
            nearby_enemies = (x for x in adjacent_points(pos) if board[x] == enemy)
            if target := min(nearby_enemies, key=lambda x: hp[x], default=None):
                hp[target] -= elf_attack if board[pos] == "E" else 3
                if hp[target] <= 0:
                    board[target] = "."
                    hp[target] = 0
                    alive[enemy] -= 1
        if alive["E"] == 0 or alive["G"] == 0:
            return hp.sum() * turn, alive
        turn += 1


board = np.array([list(x.strip()) for x in open("inputs/day15.txt")])

print(solve(board.copy(), 3)[0])

elf_attack = 4
alive_elfs = np.count_nonzero(board == "E")
while True:
    answer, alive = solve(board.copy(), elf_attack)
    if alive["E"] == alive_elfs:
        print(answer)
        break
    elf_attack += 1

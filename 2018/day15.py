import numpy as np
from collections import deque


def adjacent_cells_mask(m):
    r = np.roll(m, 1, 0) | np.roll(m, -1, 0) | np.roll(m, 1, 1) | np.roll(m, -1, 1)
    return r & ~m


def adjacent_points(pt):
    # Returned in read order.
    return ((pt[0] + y, pt[1] + x) for y, x in [(-1, 0), (0, -1), (0, 1), (1, 0)])


# Why we need to break ties: Consider this case: Once BFS reaches the @, the next
# visited spaces will be 1,2,3,4, in that order, with 3 and 4 being valid targets.
# Note 3 will be reached before 4 which is earlier in the reading order.
#
#  #@1#
#  423E
#  E...
#
def find_next_step(board, pt, enemy):
    inf = 0xFFFF
    visited = np.zeros_like(board, bool)
    visited[pt] = True
    queue = deque()
    # Seed the 4 initial positions. Note the 3rd element of the tuple doesn't change
    # after this - it tells us which of these directions we took to get to a point.
    for adj in adjacent_points(pt):
        if board[adj] == enemy:
            return None  # There is an enemy right there.
        if board[adj] == ".":
            queue.append((1, adj, adj))
            visited[adj] = True
    in_range = adjacent_cells_mask(board == enemy)
    selected = (inf, pt, pt)
    while queue:
        dist, pos, move = queue.popleft()
        if in_range[pos]:
            selected = min(selected, (dist, pos, move))  # Break ties.
        if dist >= selected[0]:
            continue  # We will never find a better dist.
        for adj in adjacent_points(pos):
            if board[adj] == "." and not visited[adj]:
                visited[adj] = True
                queue.append((1 + dist, adj, move))
    if selected[0] == inf:
        return None
    return selected[2]  # Return the direction that started us on this path.


def units_mask(board):
    return np.isin(board, ["E", "G"], assume_unique=True)  # Returns reading order.


def solve(board, elf_attack):
    enemy_for_unit = {"E": "G", "G": "E"}
    hp = np.zeros_like(board, int)
    hp[units_mask(board)] = 200
    rounds = 0
    alive = {"E": np.count_nonzero(board == "E"), "G": np.count_nonzero(board == "G")}
    while True:
        # We use a dict as an ordered set. We reverse since popitem is LIFO.
        turns = dict.fromkeys(map(tuple, reversed(np.argwhere(units_mask(board)))))
        while turns and alive["E"] > 0 and alive["G"] > 0:
            pos = turns.popitem()[0]
            assert board[pos] != "."
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
                    # Make sure the ghost of this target can't try to execute a turn
                    # Stuff might move into the place and have two goes.
                    turns.pop(target, None)
        if alive["E"] == 0 or alive["G"] == 0:
            # Add 1 if this was a complete round, otherwise we ended mid-round.
            complete_round = not turns
            return hp.sum() * (rounds + complete_round), alive
        rounds += 1


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

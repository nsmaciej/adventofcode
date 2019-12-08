from itertools import accumulate

moves = [1 if x == "(" else -1 for x in open("inputs/day01.txt").read().strip()]
print(sum(moves))
print(list(accumulate(moves)).index(-1) + 1)

def simulate(P, n, v):
    P[1] = n
    P[2] = v
    for i in range(0, len(P), 4):
        if P[i] == 99:
            break
        elif P[i] == 1:
            P[P[i + 3]] = P[P[i + 1]] + P[P[i + 2]]
        elif P[i] == 2:
            P[P[i + 3]] = P[P[i + 1]] * P[P[i + 2]]
    return P[0]


P = list(map(int, open("inputs/day02.txt").read().split(",")))
print(simulate(P[:], 12, 2))
for n in range(len(P)):
    for v in range(len(P)):
        if simulate(P[:], n, v) == 19690720:
            print(100 * n + v)
            break

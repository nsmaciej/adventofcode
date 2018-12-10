data = [x.strip() for x in open("day2.txt").readlines()]

for x in data:
    for y in data:
        if sum(i != j for i, j in zip(x, y)) == 1:
            print("".join(i for i, j in zip(x, y) if i == j))

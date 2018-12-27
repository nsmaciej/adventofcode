steps = int(open("inputs/day14.txt").read())
pattern = list(map(int, str(steps)))

recipes = [3, 7]
i, j = 0, 1
found = False
while not found:
    for x in map(int, str(recipes[i] + recipes[j])):
        recipes.append(x)
        if recipes[-len(pattern) :] == pattern:
            print(len(recipes) - len(pattern))
            found = True
            break

    i = (i + recipes[i] + 1) % len(recipes)
    j = (j + recipes[j] + 1) % len(recipes)

    if len(recipes) == steps + 10:
        print("".join(map(str, recipes[-10:])))

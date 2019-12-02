def fuel(x):
    return x // 3 - 2


def fuel_real(x):
    return x + fuel_real(fuel(x)) if x > 0 else 0


modules = list(map(int, open("inputs/day01.txt")))
print(sum(map(fuel, modules)))
print(sum(map(lambda x: fuel_real(fuel(x)), modules)))

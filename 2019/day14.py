from collections import namedtuple, defaultdict
import math

reactions = {}
Reaction = namedtuple("Reaction", ["product_count", "reactants"])
for line in open("inputs/day14.txt"):
    reactants, product = line.split(" => ")
    product_count, product_type = product.split()
    reactants = [x.split() for x in reactants.split(", ")]
    reactants = [(y, int(x)) for x, y in reactants]
    reactions[product_type] = Reaction(int(product_count), reactants)


def make(fuel_amount):
    needs = defaultdict(int)
    needs["FUEL"] = fuel_amount
    ore_used = 0
    while any(x > 0 for x in needs.values()):
        target = next(x for x in needs if needs[x] > 0)
        target_count = needs[target]
        per_reaction = reactions[target].product_count
        to_produce = math.ceil(target_count / per_reaction)
        needs[target] -= to_produce * per_reaction
        for i, iq in reactions[target].reactants:
            if i == "ORE":
                ore_used += iq * to_produce
            else:
                needs[i] += iq * to_produce
    return ore_used


print(make(1))
target = 1000000000000
low, high = 0, target
while low < high:
    mid = (low + high) // 2
    req = make(mid)
    if req > target:
        high = mid - 1
    elif req < target:
        low = mid
print(low)

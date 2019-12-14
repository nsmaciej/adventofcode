from collections import Counter, namedtuple, defaultdict
import math

Reaction = namedtuple("Reaction", ["product_count", "reactants"])

made = 0


def make(kind, q):
    global made
    if kind == "ORE":
        made += q
    elif stock[kind] >= q:
        stock[kind] -= q
    else:
        q -= stock[kind]
        stock[kind] = 0
        runs = math.ceil(q / reactions[kind].product_count)
        for _ in range(runs):
            for i, iq in reactions[kind].reactants:
                make(i, iq)
        stock[kind] = reactions[kind].product_count * runs - q


reactions = {}
stock = defaultdict(int)
for line in open("inputs/day14.txt"):
    reactants, product = line.split(" => ")
    product_count, product_type = product.split()
    reactants = [x.split() for x in reactants.split(", ")]
    reactants = [(y, int(x)) for x, y in reactants]
    reactions[product_type] = Reaction(int(product_count), reactants)

make("FUEL", 1)
print(made)

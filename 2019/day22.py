from aoc import *
import re

cmds = list(data(22).read().splitlines())
cards = list(range(10007))

for cmd in cmds:
    if cmd == "deal into new stack":
        cards.reverse()

    elif m := re.match("cut (-?[0-9]+)", cmd):
        val = int(m[1])
        cards = cards[val:] + cards[:val]

    elif m := re.match("deal with increment ([0-9]+)", cmd):
        val = int(m[1])
        j = 0
        new_cards = cards.copy()
        for i in range(len(cards)):
            new_cards[j] = cards[i]
            j = (j + val) % len(cards)
        cards = new_cards
print(cards.index(2019))
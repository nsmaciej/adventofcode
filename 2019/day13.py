from intcode import Vm
from aoc import *

program = data(13).read()
print(Vm(program).complete()[2::3].count(2))

game = Vm(program)
game.set_tape(0, 2)
score = 0
last_x = {}
game.run()
while game.has_output():
    for x, y, k in chunk(game.drain_output(), 3):
        if x == -1 and y == 0:
            score = k
        else:
            last_x[k] = x
    game.input((last_x[3] < last_x[4]) - (last_x[3] > last_x[4]))
    game.run()
print(score)

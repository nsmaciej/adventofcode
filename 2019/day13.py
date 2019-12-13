from intcode import Vm

program = open("inputs/day13.txt").read()

print(Vm(program).complete()[2::3].count(2))

game = Vm(program)
game.tape[0] = 2
score = 0
last_x = {}
game.run()
while game.has_output():
    output = game.drain_output()
    for i in range(0, len(output), 3):
        x, y, k = output[i : i + 3]
        if x == -1 and y == 0:
            score = k
        else:
            last_x[k] = x
    game.input((last_x[3] < last_x[4]) - (last_x[3] > last_x[4]))
    game.run()
print(score)

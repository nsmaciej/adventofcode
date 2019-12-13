from intcode import Vm
import time


program = open("inputs/day13.txt").read()
print(Vm(program).complete()[2::3].count(2))


def render():
    print("\033[2J\033[H", end="")
    print(score)
    tiles = {0: " ", 1: "#", 2: ".", 3: "^", 4: "@"}
    xs = [i[0] for i in screen]
    ys = [i[1] for i in screen]
    for x in range(min(ys), max(ys) + 1):
        print(
            "".join(tiles[screen.get((y, x), 0)] for y in range(min(xs), max(xs) + 1))
        )
    print()


game = Vm(program)
game.tape[0] = 2
screen = {}
score = 0
last_pos = {}
game.run()
while True:
    output = game.drain_output()
    for i in range(0, len(output), 3):
        x, y, k = output[i : i + 3]
        if x == -1 and y == 0:
            score = k
        else:
            last_pos[k] = (x, y)
            screen[x, y] = k
    ball_y = last_pos[4][0]
    paddle_y = last_pos[3][0]
    game.input((paddle_y < ball_y) - (paddle_y > ball_y))
    if 2 not in screen.values():
        break
    game.run()
print(score)

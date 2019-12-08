from collections import Counter
import numpy as np

data = open("inputs/day08.txt").read().strip()
width, height = 25, 6
best = 9999999999999
for i in range(0, len(data), width * height):
    c = Counter(data[i : i + width * height])
    if c["0"] < best:
        best = c["0"]
        # print(c["0"], c["1"] * c["2"])

answer = np.zeros((height, width))
answer[:] = 2
for i in range(0, len(data), width * height):
    m = np.array(list(map(int, data[i : i + width * height]))).reshape((height, width))
    # print(m)
    answer = np.where(answer == 2, m, answer)

print("P1")
print(f"{width} {height}")
print(" ".join(map(lambda x: str(int(0 if x == 1 else 1)), answer.flatten())))

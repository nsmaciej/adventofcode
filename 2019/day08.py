import numpy as np
from aoc import *

data = np.fromiter(data(8).read().strip(), int).reshape((-1, 6, 25))
best = (data == 0).sum((1, 2)).argmin()
print((data[best] == 1).sum() * (data[best] == 2).sum())
image = np.apply_along_axis(lambda x: next(i for i in x if i != 2), 0, data)
print(ocr(image))

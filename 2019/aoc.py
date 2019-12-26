import sys
from itertools import islice

# Public exports
from collections import *

# The biggest 'simple integer'
inf = 2 ** 30 - 1


def add(a, b):
    """Add two 2d points"""
    return a[0] + b[0], a[1] + b[1]


def sub(a, b):
    """Subtract two 2d points"""
    return a[0] - b[0], a[1] - b[1]


def around(p=(0, 0)):
    """Return points around a point"""
    y, x = p
    return [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]


def cls():
    """Clear the screen"""
    print("\033[2J", end="")


def turn(p, turn_right):
    """Turn left if False, right if True"""
    dy, dx = p
    return (dx, -dy) if turn_right else (-dx, dy)


def first(iterable):
    """Return the first truthy element of an iterable"""
    return next(x for x in iterable if x)


def data(day):
    """Return a day's data, allowing the user to possibly override it"""
    assert len(sys.argv) < 3
    file_name = sys.argv[1] if len(sys.argv) == 2 else f"inputs/day{day:02}.txt"
    return sys.stdin if file_name == "-" else open(file_name)


def chunk(iterable, size):
    """Chunk an iterable"""
    # Copied from Stack Overflow.
    it = iter(iterable)
    return iter(lambda: tuple(islice(it, size)), ())


def make_grid(array):
    """Return a index to value dict given an iterable yielding iterables"""
    return {(y, x): v for y, row in enumerate(array) for x, v in enumerate(row)}


def ocr(grid):
    """Tries to 'OCR' a boolean text"""
    import numpy as np

    # Create the grid.
    if isinstance(grid, dict):
        ix = np.array(list(grid.keys()))
        my, mx = ix.min(0)
        h, w = ix.max(0) - ix.min(0) + 1
        output = [[0] * w for _ in range(h)]
        for y, x in grid:
            output[y - my][x - mx] = grid[y, x]
        grid = ["".join(" #"[x] for x in row) for row in output]
    elif isinstance(grid, np.ndarray):
        grid = ["".join(" #"[x] for x in row) for row in grid]
    else:
        raise RuntimeError("cannot print grid")
    
    # Stip common leading and trailing whitespace.
    leading = min(x.find("#") for x in grid)
    trailing = max(x.rfind("#") + 1 for x in grid)
    grid = [x[leading:trailing] for x in grid]
    assert len(grid) == 6

    letters = {"".join(v).strip(): k for k, v in _letters.items()}  # Part to letter map
    result = ""
    for x in range(0, len(grid[0]), 5):
        if part := "".join(grid[y][x : x + 4] for y in range(6)).strip():
            result += letters[part]
    return result


_letters = {
    "A": [" ## ", "#  #", "#  #", "####", "#  #", "#  #"],
    "B": ["### ", "#  #", "### ", "#  #", "#  #", "### "],
    "C": [" ## ", "#  #", "#   ", "#   ", "#  #", " ## "],
    "F": ["####", "#   ", "### ", "#   ", "#   ", "#   "],
    "J": ["  ##", "   #", "   #", "   #", "#  #", " ## "],
    "K": ["#  #", "# # ", "##  ", "# # ", "# # ", "#  #"],
    "L": ["#   ", "#   ", "#   ", "#   ", "#   ", "####"],
    "P": ["### ", "#  #", "#  #", "### ", "#   ", "#   "],
    "U": ["#  #", "#  #", "#  #", "#  #", "#  #", " ## "],
    "Z": ["####", "   #", "  # ", " #  ", "#   ", "####"],
}

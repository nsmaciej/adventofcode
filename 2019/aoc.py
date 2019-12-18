import sys


def add(a, b):
    """Add two 2d points"""
    return a[0] + b[0], a[1] + b[1]


def sub(a, b):
    """Subtract two 2d points"""
    return a[0] - b[0], a[1] - b[1]


def around(p):
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


def data(day):
    """Return a day's data, allowing the user to possibly override it"""
    assert len(sys.argv) < 3
    file_name = sys.argv[1] if len(sys.argv) == 2 else f"inputs/day{day:02}.txt"
    return sys.stdin if file_name == "-" else open(file_name)


def make_grid(array):
    """Return a index to value dict given an iterable yielding iterables"""
    return {(y, x): v for y, row in enumerate(array) for x, v in enumerate(row)}


def print_grid(grid, blank=" ", mapping={}):
    """
    Tries to print a grid. The grid can either be a dict mapping indices to values or a
    two dimensional ndarray.
    """
    # I know mutable default arguments are bad but we never mutate mapping.
    import numpy as np

    if isinstance(grid, dict):
        ix = np.array(list(grid.keys()))
        my, mx = ix.min(0)
        h, w = ix.max(0) - ix.min(0) + 1
        output = [[blank] * w for _ in range(h)]
        for y, x in grid:
            value = grid[y, x]
            output[y - my][x - mx] = mapping.get(value, str(value))
        print("\n".join("".join(x) for x in output))

    elif isinstance(grid, np.ndarray):
        print("\n".join("".join(mapping.get(x, str(x)) for x in row) for row in grid))
    else:
        raise RuntimeError("cannot print grid")

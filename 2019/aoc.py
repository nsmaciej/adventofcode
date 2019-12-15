def add(a, b):
    return a[0] + b[0], a[1] + b[1]


def sub(a, b):
    return a[0] - b[0], a[1] - b[1]


def data(day):
    return open(f"inputs/day{day:02}.txt")


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

import subprocess
from time import perf_counter
import statistics
import os
import sys


def time_one(day, fast):
    start = perf_counter()
    subprocess.check_call(
        ["python3", f"day{day:02}.py"],
        stdout=subprocess.DEVNULL,
        env={**os.environ, "PY_INTCODE": str(fast)},
    )
    return perf_counter() - start


def time(t):
    return f"\033[1;31m{t:6.2f}s\033[0m" if t > 0.3 else f"{t:6.2f}s"


def get_speedup(day):
    before = statistics.mean(time_one(day, 1) for _ in range(runs))
    after = statistics.mean(time_one(day, 0) for _ in range(runs))
    print(f"Day {day:<2}  {before / after:4.1f}x ({time(before)}, {time(after)})")
    return before, after


runs = 10
days = [2, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25]
days = list(map(int, sys.argv[1:])) if len(sys.argv) > 1 else days
print(f"Using {runs} runs\n")
stats = list(map(get_speedup, days))
before = sum(x for x, _ in stats)
after = sum(y for _, y in stats)
print(f"\nOverall\t{before / after:4.1f}x ({before:6.2f}s, {after:6.2f}s)")

import subprocess
from time import perf_counter
import statistics
import os


def time_one(day, fast):
    start = perf_counter()
    subprocess.check_call(
        ["python3", f"day{day:02}.py"],
        stdout=subprocess.DEVNULL,
        env={**os.environ, "PY_INTCODE": str(fast)},
    )
    return perf_counter() - start


def get_speedup(day):
    before = statistics.mean(time_one(day, 1) for _ in range(runs))
    after = statistics.mean(time_one(day, 0) for _ in range(runs))
    print(f"Day {day}\t{before / after:4.1f}x")
    return before, after


runs = 10
days = [2, 7, 9, 11, 13]
print(f"Using {runs} runs\n")
stats = list(map(get_speedup, days))
before = sum(x for x, _ in stats)
after = sum(y for _, y in stats)
print(f"\nOverall\t{before / after:4.1f}x")

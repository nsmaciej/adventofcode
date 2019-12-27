import subprocess
from time import perf_counter
import statistics
import os
import sys


def time(day, slow):
    times = []
    for i in range(runs):
        start = perf_counter()
        subprocess.check_call(
            ["python3", f"day{day:02}.py"],
            stdout=subprocess.DEVNULL,
            env={**os.environ, "PY_INTCODE": str(slow)},
        )
        times.append(perf_counter() - start)
    return statistics.mean(times)


def show(t, threshold=0.3):
    return f"\033[1;31m{t:5.2f}s\033[0m" if t > threshold else f"{t:5.2f}s"


def test(day):
    result = time(day, 0)
    if day in intcode_days:
        before = time(day, 1)
        print(f"Day {day:>2} {show(result)}   ({before / result:4.1f}x {show(before)})")
    else:
        print(f"Day {day:>2} {show(result)}")
    return result


runs = 2
intcode_days = [2, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25]
test_days = map(int, sys.argv[1:]) if len(sys.argv) > 1 else range(1, 26)
print(f"Using {runs} runs\n")
total = sum(map(test, test_days))
print(f"\nTotal  {show(total, threshold=10)}")

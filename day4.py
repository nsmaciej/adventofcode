import re
import collections


def events(log):
    Event = collections.namedtuple("Event", "date, time, action")
    for entry in log:
        day = re.search(r"\[(.*)\]", entry)[1]
        time = int(re.search(r"\d\d:(\d\d)", entry)[1])
        if "falls asleep" in entry:
            yield Event(day, time, "sleep")
        elif "wakes up" in entry:
            yield Event(day, time, "wake")
        else:
            yield Event(day, time, int(re.search(r"#(\d+)", entry)[1]))


log = sorted(events(open("day4.txt").readlines()))
asleep = collections.defaultdict(lambda: [0] * 60)
guard = None
sleep_time = 0
for evt in log:
    if evt.action == "sleep":
        sleep_time = evt.time
    elif evt.action == "wake":
        for t in range(sleep_time, evt.time):
            asleep[guard][t] += 1
    else:
        guard = evt.action

print("Strategy 1")
sleepy_guard = max(asleep.keys(), key=lambda x: sum(asleep[x]))
sleep_minute = asleep[sleepy_guard].index(max(asleep[sleepy_guard]))
print(sleep_minute * sleepy_guard)

print("Strategy 2")
sleepy_guard = max(asleep.keys(), key=lambda x: max(asleep[x]))
sleep_minute = max(range(60), key=lambda x: asleep[sleepy_guard][x])
print(sleep_minute * sleepy_guard)

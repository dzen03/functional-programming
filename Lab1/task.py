import math
from functools import reduce

def smallest_multiple(arr: list):
  return reduce(math.lcm, arr)

assert smallest_multiple(list(range(1, 11))) == 2520
assert smallest_multiple(list(range(1, 21))) == 232792560

def find_cycle_length(d):
    remainders = {}
    remainder = 1
    position = 0

    while remainder != 0 and remainder not in remainders:
        remainders[remainder] = position
        remainder = (remainder * 10) % d
        position += 1

    if remainder != 0:
        return position - remainders[remainder]
    else:
        return 0

def find_max_cycle(limit):
    max_cycle = 0
    max_d = 0
    for d in range(2, limit):
        cycle_length = find_cycle_length(d)
        if cycle_length > max_cycle:
            max_cycle = cycle_length
            max_d = d
    return max_d

assert find_max_cycle(100) == 97
assert find_max_cycle(1000) == 983

print("all ok")

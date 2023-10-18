def find_quadruplet_sum(numbers, target):
    '''
    Finds four integers within `numbers` whose sum amounts to
    exactly `target`, and returns them.

    There will always be a valid quadruplet, and the same number
    can be picked several times.
    '''
    for a in numbers:
        for b in numbers:
            for c in numbers:
                for d in numbers:
                    if a+b+c+d == target:
                        return (a, b, c, d)


# we currently have an O(n^4) solution
# how can we speed it up?
# simple optimization first
# we can also eliminate a loop since we have 1 degree of freedom
# we can knock out numbers that are > remaining
# also remove duplicates and 0

nums = {n for n in exnums if n != 0}

remaining = extarget
for a in nums:
    if a < remaining:
        for b in nums:
            if b < remaining:
                for c in nums:
                    if c < remaining:
                        if (a + b + c) - remaining == 0:
                            print (a,b, c, remaining)

# so now we have O(n^3)
# how can we lower it even further?

def find_quadruplet_sum_fast(numbers, target):
    nums = {n for n in exnums if n != 0}

    for a in nums:
        for b in nums:
            for c in nums:
                buildup = target - (a + b + c)
                if buildup in nums:
                    return (a, b, c, buildup)

exnums = [5, 4, 3, 2, 1, 0]
extarget = 11

print(find_quadruplet_sum_fast(exnums, extarget))

"""
idea

start with target
if element < remaining subtract it
else iterate

this could be problematic if numbers are out of order
perhaps we can sort first

some other considerations
what if numbers are negative?
what if the sum is 0? [pre-check]
what if there are duplicate values? [set]
"""
from functools import reduce
from operator import add

exnums_sorted = set(sorted(exnums, reverse=True))
remaining = extarget
path = []

if reduce(add, exnums_sorted) == 0:
    return []

for element in exnums_sorted:
    if element <= remaining:
        remaining -= element
        path.append(element)
    else:
        continue

print(path)


"""
this solution isn't great because it is greedy, but not optimal
for this kind of solution we need some kind of backtracking

ok, so depth first search with an early stopping
"""

nums = exnums
paths = []
target = extarget

def remove(ls, v):
    return [ele for ele in ls if ele != v]

nums = remove(nums, 0)

def find_quadruplet_sum(nums: list[int], target: int, path: list[int]):
    print(f"{target=} {nums=} {path=}")

    if target == 0 and len(path) == 4:
        return path

    # now we want to test each element
    if target in nums:
        return [target] + path
    else:
        return [find_quadruplet_sum(nums, target - ele, path + [ele])
                for ele in nums]



# also not good, doesn't handle replacement
find_quadruplet_sum(remove(exnums, 0), extarget, [])

# use in to see if needed val is in the list


"""

think simpler

if target in list:
  return path + [list]
elif len(path) > 4:
  return []
else:
  return [fn(e) for e in nums]
"""

def find_quadruplet_sum(nums, target, path, valid_paths = []):
    if valid_paths:
        return valid_paths[0]
    if target in nums and len(path) == 3:
        valid_paths = path + [target]
    else:
        return [find_quadruplet_sum(nums, target - ele, path + [ele]) for ele in nums if ele < target]

find_quadruplet_sum(remove(exnums, 0), extarget, [])
print(valid_paths)

"""
maybe get rid of the extras and have early breaking by using a while loop
"""

nums = remove(exnums, 0)

def find_quadruplet_sum_fast(nums: list[int], target: int) -> list[int]:
    def get_path(nums: list[int], remaining: int, path: list[int] = []):
        if valid_paths:
            return valid_paths
        if remaining in nums and len(path) == 3:
            valid_paths.append(path + [remaining])
        else:
            return [get_path(nums, remaining - ele, path + [ele]) for ele in nums if ele < remaining]

    nums = {n for n in nums if n != 0}  # remove 0 and duplicates since we replace

    valid_paths = []
    while not valid_paths:
        get_path(nums, target)

    return valid_paths[0]

print(find_quadruplet_sum(exnums, extarget))


# try with itertools
from itertools import dropwhile, combinations_with_replacement
from functools import reduce
from operator import add

def find_quadruplet_sum_fast(nums, target):
    return next(dropwhile(lambda ls: reduce(add, ls) != target,
                          combinations_with_replacement(nums, 4)))

find_quadruplet_sum_fast(exnums, extarget)

#



# =============== DO NOT EDIT BELOW THIS LINE ===============
import random
import sys
import time

def run_testcase(numbers, target, testcase_name):
    print(testcase_name.ljust(25), end='- ')
    sys.stdout.flush()
    t0 = time.time()
    result = find_quadruplet_sum_fast(numbers, target)
    elapsed = time.time() - t0

    if type(result) not in (tuple, list):
        print(f'FAILED: the function returned {result} of type {type(result)}, not a tuple or list.')
        sys.exit(1)

    if len(result) != 4:
        print(f'FAILED: the result has {len(result)} elements, not 4')
        sys.exit(1)

    if sum(result) != target:
        print(f'FAILED: the sum of {result} is {sum(result)}, not {target}')
        sys.exit(1)

    if any(r not in numbers for r in result):
        print('FAILED: one of the numbers is not in the list')
        sys.exit(1)

    print(f'PASSED')

run_testcase([5, 4, 3, 2, 1, 0], 11, 'Small testcase')
run_testcase([54, 3, 42, 16, 4, 24], 90, 'Solution with duplicates')
run_testcase([89, -62, -92, -37, 28, 29], -7, 'With negative numbers')
run_testcase([39, -57, -53, -79, 83, -6, 27, -97], 0, 'Target is zero')

for i in range(1, 6):
    numbers = random.sample(range(-100_000_000, 100_000_000), 1000)
    target = sum(numbers[-4:])  # Make sure the target can be done by summing the last 4 numbers
    random.shuffle(numbers)  # Shuffle the list to avoid cheaters who just return the last 4 elements ;)
    run_testcase(numbers, target, f'Large test #{i}')

print('Congratulations. You passed all testcases!')

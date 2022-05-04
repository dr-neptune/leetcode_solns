gas = [1, 2, 3, 4, 5]
cost = [3, 4, 5, 1, 2]

# naive
# check the first gas station.
# add 1 gas, and see if you can travel to the next station. If so, repeat
# if not, shift gas and cost
from typing import List
from itertools import compress

def left_shift_ls(ls: List[int], n: int) -> List[int]:
    return ls[n:] + ls[:n]

def check_valid_path(gas, cost):
    total_gas = 0
    for g, c in zip(gas, cost):
        total_gas += g - c
        if total_gas < 1:
            return False
    return True

class Solution:
    def canCompleteCircuit(self, gas: List[int], cost: List[int]) -> int:
        path_validity = [check_valid_path(left_shift_ls(gas, i),
                                          left_shift_ls(cost, i)) for i in range(len(gas) - 1)]
        index = [idx for idx, i in enumerate(path_validity) if i]
        if index:
            return index[0]
        else:
            return -1


path_validity = [check_valid_path(left_shift_ls([1, 2, 3, 4, 5], i),
                                  left_shift_ls([3, 4, 5, 1, 2], i)) for i in range(len(gas) - 1)]

print(path_validity)

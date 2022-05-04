prices = [7, 1, 5, 3, 6, 4]

# brute force
# take a sliding window of size 2
# take the max of all the differences
# then take a sliding window of size 3
# ...
# then take a sliding window of size n
from typing import List, Tuple, Any

def sliding_window(ls: List[int], window: int) -> List[Tuple[int]]:
    pairs = []
    for i in range(len(ls) - window + 1):
        pairs.append(ls[i:i+window])
    return pairs

def distance(pair: List[int]) -> int:
    return pair[1] - pair[0]

def flatten_list(ls: List[Any]):
    if type(ls[0]) == list:
        new_ls = reduce(add, ls)
        if type(new_ls[0]) == list:
            return flatten_list(new_ls)
        else:
            return new_ls

exnested = list(map(lambda r: sliding_window(prices, r), range(2, len(prices))))

max([distance(p) for p in sliding_window(flatten_list(exnested), 2)])

# better solution
def max_profit(prices):
    max_profit = 0
    for idx in range(1, len(prices)):
        if prices[idx-1] < prices[idx]:
            max_profit += prices[idx] - prices[idx-1]
    return max_profit

max_profit(prices)

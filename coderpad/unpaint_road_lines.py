"""

idea

we want to get a list of intervals that are painted

later on we can optimize for bonus 2

simplest way first
we can take each pair and make a tuple
then for the next pair, we search our list of existing tuples and compare our new tuple to the existing ones.
we have some cases:
 < < : replace 1 with 1
 < > : replace 1 with 1 and 2 with 2
 > < : do nothing, as the tuple is consumed
 > > : check if a1 is larger than b2. if so, continue iteration. If no elements left, insert these values

this gets nested

 < < : check if a1 < b1 and a2 < b2. If so, check if a2 < b1. If so, prepend tuple
 < > : a1 < b1 and a2 > b2. replace b1, b2 with a1, a2
 > < : a1 > b1 and a2 < b2. do nothing
 > > : check if a1 > b1 and a2 > b2. If so, check if a1 > b2. If so, move onto the next value. If no next, append. If not, a1 gets absorbed and replace b2 with a2

"""

exin = (3, 10, 14, 20, 1, 5)

# first, sliding window
# assume we always have an even number of elements

pairs = []
for i in range(0, len(exin), 2):
    pair = (exin[i], exin[i+1])
    pairs.append(pair)
pairs = sorted(pairs)

# list replace
def replace(ls, v, replacement):
    new_ls = []
    for e in ls:
        if e == v:
            new_ls.append(replacement)
        else:
            new_ls.append(e)
    return new_ls

def remove(ls, v):
    return [e for e in ls if e != v]

# now we want to reduce
for idx, pair_a in enumerate(pairs):
    for pair_b in pairs:
        a1, b1, a2, b2 = pair_a, pair_b

        if a1 < a2 and b1 < b2:
            if a2 < b1:
                pairs.insert(idx-1, pair_a)
                pairs = remove(pairs, pair_b)
                print(f"new pairs: {pairs}")
            else:
                pairs = replace(pairs, pair_b, pair_a)



"""
if sorted, we only need to compare current tuple and next tuple

if not next tuple, then we are done

a1 b1 a2 b2
< < - leave it be
> > - check if a1 > b2. If so, swap places. If not replace b2 with b1 and drop pair a
< > - replace b1 with a1 and replace b2 with a2
> < - drop pair a
"""

new_pairs = []
for idx in range(len(pairs) - 1):
    print(pairs[idx], pairs[idx + 1])

    match pairs[idx], pairs[idx + 1]:
        case [(a1, b1), (a2, b2)] if a1 < a2 and b1 < b2:
            new_pairs.append([a1, b2])
        case [(a1, b1), (a2, b2)] if a1 > a2 and b1 > b2:
             if a1 > b2:
                 new_pairs.append([a2, b2])
                 new_pairs.append([a1, b1])
             else:
                 new_pairs.append([a2, b1])
        case [(a1, b1), (a2, b2)] if a1 < a2 and b1 < b2:
            pass
        case [(a1, b1), (a2, b2)] if a1 < a2 and b1 < b2:
            pass


"""

we have pairs
we want to perform a possible reduce on the first 2
then we want to compare our output to the next value

this sounds like a jerb for reduce
"""

from functools import reduce
from itertools import accumulate

def agglomerate(t1, t2):
    a1, b1 = t1
    a2, b2 = t2

    if a1 > a2 and b1 > b2:
        return (a2, b1)
    elif a1 < a2 and b1 < b2:
        return (a1, b2)
    elif a1 > a2 and b1 < b2:
        return (a2, b2)
    else:  # a1 < a2 and b1 > b2
        return (a1, b2)
    return t2

list(accumulate(pairs, agglomerate))


"""

try again, split into 2 lists

what if we enumerated out the intermediate steps
then we sorted the list

start with an array from 1 -> max
then split into 2 lists, starts and ends
for each start -> end iter, add 1 to our array

"""

paint_counter = [0 for _ in range(1, max(pairs, key=lambda x: x[1])[1] + 1)]

ls1, ls2 = map(sorted, zip(*pairs))

for start, end in zip(ls1, ls2):
    for i in range(start-1, end):
        print(i)
        paint_counter[i] += 1

# now we want to partition our new array into chunks, split by 0

new_pairs = []
min_element, max_element = 0, 0
# consider indices that start at 0!
for idx, element in enumerate(paint_counter):
    if element != 0:
        max_element = idx
    else:
        new_pairs.append((min_element, max_element))
        min_element = idx

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

from itertools import groupby

exin = (3, 10, 14, 20, 1, 5)

pairs = []
for i in range(0, len(exin), 2):
    pair = (exin[i], exin[i+1])
    pairs.append(pair)
pairs = sorted(pairs)

paint_counter = [0 for _ in range(1, max(pairs, key=lambda x: x[1])[1] + 1)]

ls1, ls2 = map(sorted, zip(*pairs))

for start, end in zip(ls1, ls2):
    for i in range(start-1, end):
        paint_counter[i] += 1

partitions = [list(g) for k, g in groupby(paint_counter)]

# agglomerate the partitions
# then check if > 5 overlaps
partitions = [[1, 1], [2, 2, 2], [1, 1, 1, 1, 1], [0, 0, 0], [1, 1, 1, 1, 1, 1, 1]]

def combine_if_not_zero(a, b):
    if a[0] != 0 and b[0] != 0:
        return a + b
    else:
        return a + [b]

reduce(combine_if_not_zero, partitions)


# counts = []
# num_min, num_max = 1, 0
# for ls in partitions:
#     if ls[0] != 0 and ls[-1] != 0:
#         num_max += len(ls)
#     elif ls[0] == 0:
#         counts.append([num_min, num_max])
#         num_min = num_max + 1 + len(ls)
#         num_max = num_min - 1

# last_pair = [num_min, num_max]
# if not last_pair in counts:
#     counts.append(last_pair)

# print(counts)

# try tests

# def analyze_paint_logs(logs):
#     pairs = []
#     for i in range(0, len(logs), 2):
#         pair = (logs[i], logs[i+1])
#         pairs.append(pair)
#     pairs = sorted(pairs)

#     paint_counter = [0 for _ in range(1, max(pairs, key=lambda x: x[1])[1] + 1)]

#     ls1, ls2 = map(sorted, zip(*pairs))

#     for start, end in zip(ls1, ls2):
#         for i in range(start-1, end):
#             paint_counter[i] += 1

#     partitions = [list(g) for k, g in groupby(paint_counter)]

#     counts = []
#     num_min, num_max = 1, 0
#     for ls in partitions:
#         if ls[0] != 0 and ls[-1] != 0:
#             num_max += len(ls)
#         elif ls[0] == 0:
#             counts.append([num_min, num_max])
#             num_min = num_max + 1 + len(ls)
#             num_max = num_min - 1

#     last_pair = [num_min, num_max]
#     if not last_pair in counts:
#         counts.append(last_pair)

#     return counts


analyze_paint_logs(exin)


def main():
    unpaint_instructions = analyze_paint_logs((3, 10, 14, 20, 1, 5))
    print(unpaint_instructions)
    assert unpaint_instructions == [1, 1, 10, 1, 14, 20]

    unpaint_instructions = analyze_paint_logs((1, 7, 1, 7, 1, 11, 1, 7, 1, 7))
    print(unpaint_instructions)
    assert unpaint_instructions == [5, 1, 7, 1, 7, 11]

    unpaint_instructions = analyze_paint_logs(
        (
            5.2, 10.7, 5.3, 10.6, 5.0, 10.9, 5.1, 10.8,
            7.7, 8.8, 6.6, 7.7, 7.0, 8.0,
            1.5, 2.3, 1.6, 2.1, 2.3, 3.4,
        )
    )
    print(unpaint_instructions)
    assert unpaint_instructions == [
        1, 1.5, 3.4,
        1, 5.0, 6.6,
        5, 6.6, 8.8,
        1, 8.8, 10.9
    ]

main()



# def analyze_paint_logs(coords):
#     painted = [False for _ in range(max(coords) + 1)] # Add 1 to guarantee unpainted last cell

#     # Naive painting algorithm
#     for coord_i in range(0, len(coords), 2):
#         start = coords[coord_i]
#         end = coords[coord_i + 1]
#         for paint_idx in range(start, end):
#             painted[paint_idx] = True

#     previous_painted = False
#     interval_start = None
#     result = []
#     for i in range(len(painted)):
#         if painted[i] and not previous_painted:
#             # Found new interval start
#             interval_start = i
#         if not painted[i] and previous_painted:
#             # Found current interval end
#             result.extend([interval_start, i])

#         # Remember previous cell state
#         previous_painted = painted[i]

#     return tuple(result)


# analyze_paint_logs(exin)

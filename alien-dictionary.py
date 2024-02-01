from collections import defaultdict, Counter, deque

words = ["wrt","wrf","er","ett","rftt"]

adj_list = defaultdict(set)

in_degree = Counter({c: 0 for word in words for c in word})

for first_word, second_word in zip(words, words[1:]):
    for c, d in zip(first_word, second_word):
        print(f"c: {c} d: {d}")


def populate_dicts(words):
    adj_list = defaultdict(set)
    in_degree = Counter({c : 0 for word in words for c in word})

    # Step 1: We need to populate adj_list and in_degree.
    # For each pair of adjacent words...
    for first_word, second_word in zip(words, words[1:]):
        for c, d in zip(first_word, second_word):
            if c != d:
                if d not in adj_list[c]:
                    adj_list[c].add(d)
                    in_degree[d] += 1
                break
            else: # Check that second word isn't a prefix of first word.
                if len(second_word) < len(first_word): return ""
    return in_degree, adj_list



in_degree, adj_list = populate_dicts(words)

(Counter({'r': 1, 't': 1, 'f': 1, 'e': 1, 'w': 0}),
 defaultdict(<class 'set'>, {'t': {'f'}, 'w': {'e'}, 'r': {'t'}, 'e': {'r'}}))


[c for c in in_degree if in_degree[c] == 0]

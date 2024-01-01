from collections import Counter

class Solution:
    @staticmethod
    def all_members(ls_counter: dict[str, int], elements_counter: dict[str, int]) -> bool:
        for ele in elements_counter.keys():
            ls_val = ls_counter.get(ele, 0)
            ele_val = elements_counter.get(ele)

            if ls_val < ele_val:
                return False
        return True

    def minWindow(self, s: str, t: str) -> str:
        t_counts = Counter(t)
        default_value = " " * 10**5
        answer = default_value
        lhs, rhs = 0, 0
        window = []
        window_counter = {}

        while rhs < len(s):
            curr_char = s[rhs]
            window_counter[curr_char] = window_counter.get(curr_char, 0) + 1
            window.append(curr_char)
            # window += curr_char

            while self.all_members(window_counter, t_counts):
                if len(window) < len(answer):
                    answer = window

                rem_char = s[lhs]
                window_counter[rem_char] = window_counter.get(rem_char) - 1
                window = window[1:]
                lhs += 1
            else:
                rhs += 1

        if default_value == answer:
            return ""

        return "".join(answer)


Solution().minWindow("adobecodebanc", "abc")
Solution().minWindow("a", "aa")
Solution().minWindow("a", "a")
Solution().minWindow("bba", "ab")
Solution().minWindow("acbbaca", "aba")

"""hella slow"""

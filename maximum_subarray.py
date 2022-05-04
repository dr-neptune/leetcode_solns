def maxSubArray(nums, best=-10000, current=0):
    if len(nums) == 0:
        return best
    else:
        fnum = nums[0]
        current_sum = max(fnum, current + fnum)
        return maxSubArray(nums[1:], max(best, current_sum), current)

maxSubArray(exin)

def maxSubArray(nums):
    current_subarray = max_subarray = nums[0]
    for num in nums[1:]:
        # if the current subarray is negative, throw it away. ow keep adding to it
        current_subarray = max(num, current_subarray + num)
        max_subarray = max(max_subarray, current_subarray)
    return max_subarray

maxSubArray(exin)


exin = [-2,1,-3,4,-1,2,1,-5,4]

def maxSubArray(nums):
    max_val = curr_seq = nums[0]
    for num in nums[1:]:
        curr_seq = max(curr_seq + num, num)
        max_val = max(max_val, curr_seq)
    return max_val


def max_sub_array(nums):
    max_val = curr_seq = nums[0]
    for num in nums[1:]:
        curr_seq = max(curr_seq + num, num)
        max_val = max(max_val, curr_seq)
    return max_val

max_sub_array(exin)

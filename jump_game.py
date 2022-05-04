nums = [2, 3, 1, 1, 4]

# backtracking
# start with 2
# see if we can reach 4 by going forward 1 and 2
# then see if we can reach 4 by going forward 3, 2, 1
def jump(nums):
    if len(nums) == 1:
        return 1
    else:
        fele = nums[0]
        print(fele)
        if fele == 0:
            return 0
        else:
            for i in [range(1, fele+1)]:
                return jump(nums[i:])

jump(nums)

def jump(nums):
    first_element = nums[0]
    if first_element


# idea
# start a pointer at the first and last value
# calculate the area
# if a > max, set max to a
# if lhs > rhs, set rhs to rhs - 1

height = [1,8,6,2,5,4,8,3,7]

def max_area(height):
    def calc_area(a, b, dist):
        # the area is the distance on the x axis times the min of a, b
        return dist * min(a, b)
    def ptr_narrow(left, right, max_val):
        if left >= right:
            return max_val
        v_left, v_right = height[left], height[right]
        amt = calc_area(v_left, v_right, right - left)
        max_val = max(max_val, amt)
        if v_left > v_right:
            return ptr_narrow(left, right - 1, max_val)
        else:
            return ptr_narrow(left + 1, right, max_val)
    return ptr_narrow(0, len(height) - 1, 0)



print(max_area(height))

print(max_area([1, 1]))

print(max_area([2, 1]))

# Day 2
# Read in data 
dat = open("/Users/christina/Documents/adventofcode_2020/Day_2/day_2_dat.txt").readlines()

# Part 1
i = 0 
for x in dat: 
    nums, letter, password = x.split()
    nums = [int(x) for x in nums.split("-")]
    letter = letter[0]
    n = password.count(letter)
    if nums[0] <= n <= nums[1]:
        i = i + 1

print(i)   

# Part 2
i = 0 
y = 0 
for x in dat: 
    nums, letter, password = x.split()
    nums = [int(x)-1 for x in nums.split("-")]
    letter = letter[0]
    test1 = password[nums[0]] == letter
    test2 = password[nums[1]] == letter
    if (test1 or test2):
        i = i + 1
    if (test1 and test2):
        y = y + 1

print(i-y) 
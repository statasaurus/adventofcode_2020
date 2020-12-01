from functools import reduce
import itertools
# Read in the file
dat = open("/Users/christina/Documents/adventofcode_2020/Day_1/day_1_dat.txt").read().split("\n")
# Convert the string into a list of ints 
dat_num = [int(x) for x in dat if x != ""]

def day1_fx(val_ls, n, target=2020):
	out = itertools.combinations(dat_num, n)

	for x in out:
		x_sum = sum(x)
		if x_sum == target:
			print(reduce((lambda x, y: x * y), x))


# Part 1 
day1_fx(dat_num, 2)

# Part 2 
day1_fx(dat_num, 3)
	
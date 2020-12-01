#Part 1 
from functools import reduce
dat = open("day_1_dat.txt").read().split("\n")

# Python3 program to find all pairs in 
# a list of integers with given sum 

def findPairs(lst, K): 
	res = [] 
	while lst: 
		num = lst.pop() 
		diff = K - num 
		if diff in lst: 
			res.append((diff, num)) 
		
	res.reverse() 
	return res 

dat_num = [int(x) for x in dat if x != ""]

# Driver code 
K = 2020
answer = findPairs(dat_num, K)[0]
print(reduce((lambda x, y: x * y), answer))



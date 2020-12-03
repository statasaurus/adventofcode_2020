# Day 3
# Read in data 
dat = open("/Users/christina/Documents/adventofcode_2020/Day_3/day3_dat.txt").readlines()
dat = [x.replace("\n", "") for x in dat]

def tree_count(dat, right, down):
    # Get the slope 
    slope = down / right

    # Get the x and y corrdinates 
    total_dist = len(dat)
    loc_y = [x for x in range(total_dist) if x % down == 0]
    loc_x = [int(x/slope) for x in loc_y]

    # Count the trees 
    n_trees = 0 
    for i, ln in enumerate(dat): 
        n_repeats = max(loc_x) // len(ln) + 1
        rep_ln = ln * n_repeats
        step = int(i / down)
        if(i in loc_y and rep_ln[loc_x[step]] == "#"):
            n_trees = n_trees + 1
            
    return(n_trees)

# Part 1
print(tree_count(dat, right = 3, down = 1))

# Part 2 
track1 = tree_count(dat, right = 1, down = 1)
track2 = tree_count(dat, right = 3, down = 1)
track3 = tree_count(dat, right = 5, down = 1)
track4 = tree_count(dat, right = 7, down = 1)
track5 = tree_count(dat, right = 1, down = 2)
print(track1 * track2 * track3 * track4 * track5)



        



library(readr)
library(stringr)
library(purrr)

dat <- read_lines("Day_3/day3_dat.txt") 

map_matrix <- dat %>% 
  str_split("") %>% 
  do.call("rbind", .)

# Part 1 
n_trees <- function(map_matrix, right, down){
  path_end <- dim(map_matrix)[1]
  path_rows <- seq(1, path_end, by = down)
  path_cols <- seq(1, by = right, length.out = length(path_rows))
  
  # The minimum width 
  width = max(path_cols)
  n_reps = ceiling(width/dim(map_matrix)[2])
  
  full_map <- do.call("cbind", rep(list(map_matrix), n_reps))
  path <- map2_chr(path_rows, path_cols, function(x, y){
    full_map[x,y]
  })
  
  path %>% 
    str_detect("#") %>% 
    sum()
}

n_trees(map_matrix, right = 3, down = 1)

# Part 2 
path1 <- n_trees(map_matrix, right = 1, down = 1)
path2 <- n_trees(map_matrix, right = 3, down = 1)
path3 <- n_trees(map_matrix, right = 5, down = 1)
path4 <- n_trees(map_matrix, right = 7, down = 1)
path5 <- n_trees(map_matrix, right = 1, down = 2)
path1*path2*path3*path4*path5




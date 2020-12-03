library(readr)
library(stringr)
library(purrr)

dat <- read_table("Day_3/day3_dat.txt", col_names = FALSE) 
temp_length <- dat %>% 
  pull(X1) %>% 
  str_length() %>% 
  unique()
  
map_matrix <- dat %>% 
  mutate(split_dat = str_split(X1, "")) %>% 
  pull(split_dat) %>% 
  unlist() %>%
  matrix(., ncol = temp_length, byrow = TRUE)

path_end = dim(map_matrix)[1]

# Part 1 
right = 3 
down = 1 

path_rows <- seq(1, path_end, by = down)
path_cols <- c(1, path_rows*right+1)[-324]

# The minium width 
width = max(path_cols)
n_reps = ceiling(width/dim(map_matrix)[2])

full_map <- rep(map_matrix, n_reps) %>% 
  matrix(ncol = temp_length*n_reps)
path <- map2_chr(path_rows, path_cols, function(x, y){
  full_map[x,y]
})

path %>% 
  str_detect("#") %>% 
  sum()

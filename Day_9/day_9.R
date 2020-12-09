library(dplyr)
library(tidyr)
library(purrr)
values <- readLines("Day_9/day_9_dat.txt") %>% 
  as.numeric()

valid_test <- function(index, values = values){
  if(index < 25){
    FALSE
  } else {
    i <- index - 25
    subvec <- values[i:(i+24)]
    remove_rows <- c(1, c(1:24) * 25 +c(2:25))
    sums <- subvec %>% crossing(val1 = ., val2 = subvec) %>% 
      slice(- remove_rows) %>% 
      mutate(val = val1 + val2) %>% 
      pull(val)
    values[index] %in% sums
  }
}


# Part 1 
part_1 <- tibble(index = c(26:625), 
       test = map_lgl(c(26:625), valid_test, values)) %>% 
  filter(!test) %>% 
  pull(index) %>% 
  values[.]


# Part 2 
target_val <- part_1 
cont_n = 3 
set_found <- FALSE
answer <- c()
while(!set_found){
  max <- length(values) - cont_n
  for(i in c(1:max)){
    total <- sum(values[i:(i+cont_n-1)])
    if(total == target_val){
      set_found <- TRUE
      answer <- c(values[i:(i+cont_n-1)])
    } 
  }
  cont_n <- cont_n + 1 
}

min(answer) + max(answer)


library(dplyr)
library(stringr)
library(purrr)
dat <- readLines("Day_6/day_6_dat.txt")
customs <- dat %>% 
  if_else(str_length(.) == 0, "BREAK", .) %>%
  str_c(collapse = " ") %>% 
  str_split("BREAK")  %>% 
  map(~str_split(., "\\s")) %>% 
  flatten() %>% 
  map(~keep(., str_detect, "[a-z]"))
# Part 1 
customs %>% 
  map_int(function(x){
    x %>% 
      str_split("") %>% 
      unlist() %>% 
      unique() %>% 
      length()
    }) %>% 
  sum()

# Part 2 
customs %>% 
  map_int(function(x){
    n_party <- length(x)
    full_x <- str_c(x, collapse = "")
    answered <- str_split(x, "") %>% unlist() %>% unique()
    tibble(answered, 
           n = map_int(answered, ~str_count(full_x, .))) %>% 
      filter(n == n_party) %>% 
      nrow()
  }) %>% 
  sum()

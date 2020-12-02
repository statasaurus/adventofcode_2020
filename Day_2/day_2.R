library(readr)
library(stringr)
library(dplyr)

dat <- read_delim("Day_2/day_2_dat.txt", " ", col_names = FALSE)
# Part 1
passwords <- dat %>% 
  mutate(X2 = str_remove(X2, ":"),
         low = str_extract(X1, "^\\d{1,2}") %>% as.integer(),
         high = str_extract(X1, "\\d{1,2}$") %>% as.integer(),
         n = str_count(X3, X2)) %>%
  filter(n >= low, n <= high)
dim(passwords)[1]

# Part 2 
passwords <- dat %>% 
  mutate(X2 = str_remove(X2, ":"),
         loc1 = str_extract(X1, "^\\d{1,2}") %>% as.integer(),
         loc2 = str_extract(X1, "\\d{1,2}$") %>% as.integer(),
         test1 = str_sub(X3, start = loc1, end = loc1) == X2,
         test2 = str_sub(X3, start = loc2, end = loc2) == X2) %>% 
  filter(test1 + test2 == 1)
dim(passwords)[1]

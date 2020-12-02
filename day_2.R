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
# Part 1
dim(passwords)[1]

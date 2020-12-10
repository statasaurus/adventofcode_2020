library(purrr)
library(tidyr)
library(dplyr)
library(arrangements)
dat <- readLines("Day_10/day_10_dat.txt") %>%
  as.integer() %>%
  sort()
dat <- c(dat, max(dat) + 3) # Adding in for the final in device adapter
# Part 1
tibble(
  adapter = dat,
  diff = dat - lag(dat, default = 0)
) %>%
  filter(diff < 4) %>%
  summarise(
    diff_1 = sum(diff == 1),
    diff_3 = sum(diff == 3)
  ) %>%
  mutate(answer = diff_1 * diff_3)

# Part 2
# Get the location of numbers which are dropable
result <- tibble(
  adapter = dat,
  diff = dat - lag(dat, default = 0),
  req = diff == 3,
  lag2 = lag(dat, n = 2, default = 0),
  diff2 = dat - lag(dat, n = 2, default = 0),
  dropable = !req & lead(diff2) < 3
)

part_2 <- result %>%
  mutate(loc = row_number()) %>%
  filter(dropable) %>%
  mutate(key = cumsum(c(TRUE, diff(loc) != 1L))) %>%
  group_by(key) %>%
  summarise(seq_length = n()) %>%
  mutate(n_combs = case_when(
    seq_length == 1 ~ 2^1, # there or not
    seq_length == 2 ~ 2^2, # every comb of being there
    seq_length == 3 ~ 2^3 - 1, # remove 1 because all 3 can't be missing
    seq_length == 4 ~ 2^4 - 4
  )) %>% # remove the counts that would violate the rules
  pull(n_combs) %>% # to combine all options together
  prod()

print(part_2, digits = 16)

library(readr)
library(dplyr)
library(tidyr)

#### Part 1
data <- read_table("Day_1/day_1_dat.txt", col_names = FALSE) 
data %>% 
  mutate(X2 = X1) %>% 
  expand(X1, X2) %>% 
  mutate(val = X1 + X2,
         answer = X1*X2) %>% 
  filter(val == 2020) %>% 
  pull(answer) %>% 
  unique()
  
#### Part 2 

data %>% 
  mutate(X2 = X1,
         X3 = X1) %>% 
  expand(X1, X2, X3) %>% 
  mutate(val = X1 + X2 + X3,
         answer = X1*X2*X3) %>% 
  filter(val == 2020) %>% 
  pull(answer) %>% 
  unique()

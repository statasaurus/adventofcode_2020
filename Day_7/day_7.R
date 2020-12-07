library(readr)
library(dplyr)
library(purrr)
library(stringr)

dat <- readLines("Day_7/day_7_dat.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("bag", "inside"), sep = "contain") %>% 
  mutate(inside = str_split(inside, ","))

bag_check <- function(x, bag_vec){
  collapsed <- bag_vec %>% 
    str_trim() %>% 
    str_c(collapse = "|")
  str_detect(x, collapsed) %>% any()
}

# Part 1 
total_complete = FALSE 
bags <- c()
i = 0 
while(!total_complete){
  if(i == 0){
    contains_gold <- dat %>% 
      mutate(gold_test = map_lgl(inside, bag_check, "shiny gold")) %>% 
      filter(gold_test) %>% 
      pull(bag) %>% 
      str_remove("s?\\s$") 
    bags <- c(bags, contains_gold)
    i = i + 1
  } else {
    contains_gold <- dat %>% 
      mutate(cg_test = map_lgl(inside, bag_check, contains_gold)) %>% 
      filter(cg_test) %>% 
      pull(bag) %>% 
      str_remove("s?\\s$") 
    bags <- c(bags, contains_gold)
    i = i + 1
    if(length(contains_gold) == 0){
      total_complete <- TRUE
    }
  }
}

bags %>% 
  unique() %>% 
  length()

# Part 2

bag_translate <- function(bag_name){
  bags <- dat %>% 
    filter(str_detect(bag, bag_name)) %>% 
    pull(inside) %>% 
    unlist() %>% 
    str_trim() %>% 
    str_remove("s?\\.?$") 
  tibble(bag_from = bag_name,
         bag_to = str_remove(bags, "\\d*") %>% str_trim(), 
         n_needed = str_extract(bags, "\\d*") %>% as.integer()) %>% 
    mutate(n_needed = replace_na(n_needed, 1))
}


bags <- "shiny gold"
all_multi <- c(0)
total_complete = FALSE 
i = 0 
while(!total_complete){
  if(i == 0){
    temp <- bag_translate(bags)
    bags <- temp %>% pull(bag_to)
    multi <- temp %>% pull(n_needed)
    all_multi <- c(all_multi, multi)
    i <- i + 1
  } else {
    temp <- map2_dfr(bags, multi, function(x, y) { 
      bag_translate(x) %>% 
        mutate(n_needed = n_needed * y)
    }) %>% 
      group_by(bag_to) %>% 
      summarise(n_needed = sum(n_needed))
    temp <- temp %>% 
      filter(bag_to != "no other bag")
    bags <- temp %>% pull(bag_to)
    multi <- temp %>% pull(n_needed)
    all_multi <- c(all_multi, multi)
    i <- i + 1
    if(nrow(temp) == 0){
      total_complete = TRUE
    }
  }
}
sum(all_multi)

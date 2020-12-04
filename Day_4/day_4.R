library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

dat <- readLines("Day_4/day_4_dat.txt")
pass_dat <- dat %>% 
  if_else(str_length(.) == 0, "BREAK", .) %>%
  str_c(collapse = " ") %>% 
  str_split("BREAK") %>% 
  unlist()

valid_check <- function(passport, 
                        req_keys = c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")){
  n_missing <- req_keys %>% 
    map_lgl(~!str_detect(passport, .)) %>% 
    sum()
  if_else(n_missing > 0, FALSE, TRUE)
}

#Part 1
pass_dat %>% 
  map_lgl(valid_check) %>% 
  sum()

# Part 2 
# Complex checks 
year_check <- function(value, low, high){
  if(str_count(value, "\\d") == 4){
    test <- as.integer(value) %>% 
      between(low, high)
  } else {
    test <- FALSE
  }
  test
}


height_check <- function(value){
  unit <- str_extract(value, "in|cm")
  if(is.na(unit)){
    test <- FALSE
  } else if(unit == "in"){
    test <- str_extract(value, "\\d*(?=in)") %>% 
      as.integer() %>% 
      between(59, 76)
  } else if (unit == "cm"){
    test <- str_extract(value, "\\d*(?=cm)") %>% 
      as.integer() %>% 
      between(150, 193)
  } else {
    test <- FALSE
  }
}

hair_check <- function(value){
  str_detect(value, "(?<=#)[0-9|a-f]{6}")
}

eye_color <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

tibble(passport = pass_dat,
               valid_key = pass_dat %>% 
                 map_lgl(valid_check)) %>% 
  mutate(passport = str_split(passport, " "),
         id = row_number()) %>% 
  unnest_longer(passport) %>% 
  filter(passport != "", valid_key) %>% 
  separate(passport, c("key", "value"), sep = ":") %>% 
  rowwise()%>% 
  mutate(result = switch(key,
                         "byr" = year_check(value, 1920, 2002),
                         "iyr" = year_check(value, 2010, 2020),
                         "eyr" = year_check(value, 2020, 2030),
                         "hgt" = height_check(value),
                         "hcl" = hair_check(value),
                         "ecl" = value %in% eye_color,
                         "pid" = str_count(value, "\\d") == 9,
                         "cid" = TRUE)) %>% 
  group_by(id) %>%
  summarise(valid = all(result)) %>%
  filter(valid) %>% 
  nrow()
  

    
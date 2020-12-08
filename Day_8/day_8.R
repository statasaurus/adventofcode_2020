library(readr)
library(stringr)
dat <- readLines("Day_8/day_8_dat.txt") 

# Part 1 
get_accum <- function(dat){
  tot_acc <- 0 
  past_loc <- c()
  current_loc <- 1
  while(!current_loc %in% past_loc & current_loc <= length(dat)){
    x <- dat[current_loc]
    if(str_detect(x, "acc")){
      n <- str_extract(x, "-?\\d+") %>% as.integer()
      tot_acc <- tot_acc + n
      past_loc <- c(past_loc, current_loc)
      current_loc <- current_loc + 1
    } else if (str_detect(x, "jmp")) {
      n <- str_extract(x, "-?\\d+") %>% as.integer()
      past_loc <- c(past_loc, current_loc)
      current_loc <- current_loc + n
    } else if (str_detect(x, "nop")) {
      past_loc <- c(past_loc, current_loc)
      current_loc <- current_loc + 1
    }
  }
  list(tot_acc=tot_acc, past_loc = past_loc)
}

part_1 <- get_accum(dat)


# Part 2 
error_loc_opts <- tibble(loc = part_1$past_loc, 
       val = dat[part_1$past_loc]) %>% 
  filter(str_detect(val, "jmp|nop")) %>% 
  pull(loc)
fix_dat <- function(dat, loc){
  if(str_detect(dat[loc], "jmp")){
    dat[loc] <- str_replace(dat[loc], "jmp", "nop")
  } else {
    dat[loc] <- str_replace(dat[loc], "nop", "jmp")
  }
  dat
}

answers <- error_loc_opts %>% map_df(function(x){
  run_check <- fix_dat(dat, x) %>% 
    get_accum()
  last_index <- last(run_check$past_loc) 
  tibble(test = last_index == length(dat),
         tot_acc = run_check$tot_acc)
})

answers %>% 
  filter(test)


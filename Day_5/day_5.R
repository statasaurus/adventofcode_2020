library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# Part 1 
dat <- read_table("Day_5/day_5_dat.txt", col_names = FALSE) %>% 
  mutate(row = str_extract(X1, "[F|B]{7}"), 
         col = str_extract(X1, "[L|R]{3}"))

bi_sp_part <- function(letters, seat_vec) {
  map2(letters, seat_vec, function(x, y){
    half <- length(y)/2
    if(x == "F" | x == "L"){
      y[1:half]
    } else {
      y[(half+1):length(y)] 
    }
  }
  )
}

get_row <- function(vals){
  seat_rows <- rep(list(c(0:127)), length(vals))
  for(i in 1:7){
    letters <- str_sub(vals, i,i)
    seat_rows <- bi_sp_part(str_sub(vals, i,i), seat_rows)
  }
  seat_rows %>% 
    unlist()
}

get_col <- function(vals){
  seat_cols <- rep(list(c(0:7)), length(vals))
  for(i in 1:3){
    letters <- str_sub(vals, i,i)
    seat_cols <- bi_sp_part(str_sub(vals, i,i), seat_cols)
  }
  seat_cols %>% 
    unlist()
}

seat_info <- dat %>% 
  mutate(row_num = get_row(row), 
         col_num = get_col(col),
         seat_id = row_num * 8 + col_num)
seat_info %>% 
  summarise(part_1 = max(seat_id))

# Part 2 
seat_above <- seat_info %>% 
  arrange(seat_id) %>% 
  mutate(seat_check = seat_id - lag(seat_id)) %>% 
  filter(seat_check > 1) %>% 
  pull(seat_id) 
seat_above-1

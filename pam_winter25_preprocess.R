library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)

D <- LoadFromDirectory("data/", event="Game")


#############
# Format D
#############

D <- D %>% rename(Game = i3, Participant = i2, ConditionLabel = Condition, Condition = i4)

col_formats = read.csv("pam_column.csv", sep=";")
col_formats = col_formats %>% filter(name %in% colnames(D))

D = D %>% 
  mutate_at(col_formats %>% pull(name), 
            ~ifelse(.x == "NULL", NA, .x)) %>%
  mutate_at(col_formats %>% filter(type=="numeric") %>% pull(name), 
            ~as.numeric(.x)) %>%
  mutate_at(col_formats %>% filter(type=="int") %>% pull(name), 
            ~as.integer(.x)) %>%
  mutate_at(col_formats %>% filter(type=="time") %>% pull(name), 
            ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%OS")) 



#############
# Load Likert Data
#############

L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1_4b7ZV6QOW0KvtCOBV0L4MxS7sf-vq2r4WkrJ-haSTI/edit?gid=237988817#gid=237988817')
col_formats = read.csv("likert_column.csv", sep=";")

col_formats = col_formats %>% filter(name %in% colnames(L))

L = L %>% 
  mutate_at(col_formats %>% pull(name), 
            ~ifelse(.x == "NULL", NA, .x)) %>%
  mutate_at(col_formats %>% filter(type=="numeric") %>% pull(name), 
            ~as.numeric(.x)) %>%
  mutate_at(col_formats %>% filter(type=="int") %>% pull(name), 
            ~as.integer(.x)) %>%
  mutate_at(col_formats %>% filter(type=="time") %>% pull(name), 
            ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%OS")) 

D <- D %>% left_join(L, by=c('Game' = 'Game', 'Condition' = 'Condition', 'Participant' = 'Participant'))

#############
# Save to RDA
#############
save(D, file = 'data_all.rda', compress=TRUE)

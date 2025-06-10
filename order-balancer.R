library(plotly)
library(sf)
library(gsheet)
library(interp)
library(gtools)
library(tidyverse)
# Check Latin Square balance

Lf <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1_4b7ZV6QOW0KvtCOBV0L4MxS7sf-vq2r4WkrJ-haSTI/edit?gid=237988817#gid=237988817')

### 1: generate all 

generate_possibilities <- function(df, target_var, order_var) {
    expand.grid(unique(df[[target_var]]), unique(df[[order_var]])) %>%
      mutate(occurence = paste(Var1,Var2,sep="-"),
             target = Var1,
             order = Var2,
             n = 0,
             Participant = NA,
             combination = NA) %>% select(-Var1, -Var2)
}

count_occurences <- function(df, target_var, order_var, pid_var) {
  data = data.frame(target = df[[target_var]],
                  order = df[[order_var]],
                  pid = df[[pid_var]])
  data %>% group_by(pid) %>% arrange(order) %>%
    mutate(occurence = paste(target,order, sep="-"),
           target = target,
           order = order,
           combination = paste(target, collapse="-")) %>%
    group_by(pid, occurence) %>% 
    summarize(n(),
              order = unique(order),
              target = unique(target),
              combination = unique(combination)) %>% group_by(pid, occurence) %>%
    summarize(n = n(),
              order = unique(order),
              target = unique(target),
              combination = unique(combination)) %>%
    rename(Participant = pid)
}

# Overview of each participant's placement.
PAMCondition = generate_possibilities(Lf %>% filter(Game == "PAM"),'Condition','Order') %>% 
  rbind(count_occurences(Lf %>% filter(Game == "PAM"),'Condition','Order','Participant')) %>%
  arrange(Participant)

KiwiCondition = generate_possibilities(Lf %>% filter(Game == "Kiwi"),'Condition','Order') %>% 
  rbind(count_occurences(Lf %>% filter(Game == "Kiwi"),'Condition','Order','Participant')) %>%
  arrange(Participant)


KiwiColor = generate_possibilities(Lf %>% filter(Game == "Kiwi"),'Color','Order') %>% 
  rbind(count_occurences(Lf %>% filter(Game == "Kiwi"),'Color','Order','Participant')) %>%
  arrange(Participant)

KiwiConditionColor = generate_possibilities(Lf %>% filter(Game == "Kiwi"),'Color','Condition') %>% 
  rbind(count_occurences(Lf %>% filter(Game == "Kiwi"),'Color','Condition','Participant')) %>%
  arrange(Participant)

# Summary of number of times each thing was at each position

make_counts <- function(df, title = "") {
 df2 = df %>% ungroup() %>%
    group_by(occurence) %>%
    summarise(n = sum(n),
              order = unique(order))
 
 names(df2)[1] = title
 #names(df2)[2] = paste0(title,"Count")
 
 return(df2)
}

make_counts(PAMCondition, "PAMCondition") %>%
  arrange(order, n)

# PAMCombination?


make_counts(KiwiCondition, "KiwiCondition") %>%
  arrange(order, n) #%>%
  #group_by(order) %>%
  #filter(row_number() %in% c(1,2,3))
  
make_counts(KiwiColor,"KiwiColor") %>%
  arrange(order, n) %>%
  group_by(order)

make_counts(KiwiConditionColor,"KiwiConditionColor") %>%
  arrange(order, n) %>%
  group_by(order)
